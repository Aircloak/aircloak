defmodule Air.Service.Explorer do
  @moduledoc """
  Implements a service for interacting with a Diffix Explorer service.
  """
  use GenServer
  alias Air.Repo
  alias Air.Schemas.{DataSource, ExplorerAnalysis}
  require Aircloak.DeployConfig
  import Ecto.Query
  import Aircloak, only: [in_env: 1]
  require Logger
  alias Air.Service.{User, Token, Group}

  @poll_interval in_env(test: 0, else: :timer.seconds(5))

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @spec start_link(any) :: :ignore | {:error, any} | {:ok, pid}
  def start_link(_), do: GenServer.start_link(__MODULE__, [], name: __MODULE__)

  @doc "Is the Diffix Explorer integration enabled? Other methods should only be called if this returns `true`."
  @spec enabled? :: boolean
  def enabled?() do
    case Aircloak.DeployConfig.fetch("explorer") do
      {:ok, _} -> true
      :error -> false
    end
  end

  @doc "Is diffix explorer integration enabled for this datasource?"
  @spec data_source_enabled?(DataSource.t()) :: boolean
  def data_source_enabled?(ds) do
    {user, _} = find_or_create_explorer_creds()
    enabled?() and match?({:ok, _}, Air.Service.DataSource.fetch_as_user({:id, ds.id}, user))
  end

  @doc "Returns analysis results for a particular data source"
  @spec results_for_datasource(DataSource.t()) :: [ExplorerAnalysis.t()]
  def results_for_datasource(ds),
    do:
      Repo.all(
        from(ea in ExplorerAnalysis,
          where: ea.data_source_id == ^ds.id,
          preload: [:data_source]
        )
      )

  @doc "Deletes all analysis results for the datasource and requests new analyses."
  @spec reanalyze_datasource(Air.Schemas.DataSource.t()) :: :ok
  def reanalyze_datasource(ds) do
    Repo.delete_all(
      from(ea in ExplorerAnalysis,
        where: ea.data_source_id == ^ds.id
      )
    )

    GenServer.call(__MODULE__, {:request_analysis_for_data_source, ds})
  end

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(_) do
    if enabled?() do
      _ = find_or_create_explorer_creds()
      Process.send_after(self(), :poll, @poll_interval)
      {:ok, true}
    else
      {:ok, false}
    end
  end

  @impl GenServer
  def handle_call({:request_analysis_for_data_source, data_source}, _from, poll_in_progress?) do
    request_analysis_for_data_source(data_source)
    unless poll_in_progress?, do: Process.send_after(self(), :poll, @poll_interval)
    {:reply, :ok, true}
  end

  @impl GenServer
  def handle_info(:poll, _state) do
    pending_analyses_query()
    |> Repo.all()
    |> Enum.each(&poll_for_update/1)

    if Repo.exists?(pending_analyses_query()) do
      Process.send_after(self(), :poll, @poll_interval)
      {:noreply, true}
    else
      {:noreply, false}
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp pending_analyses_query() do
    from(ea in ExplorerAnalysis, where: ea.status in ["new", "processing"])
  end

  defp find_or_create_explorer_creds() do
    login = "diffix-explorer@aircloak.com"
    token_desc = "Used by Diffix Explorer to issue queries"
    name = "Diffix Explorer"

    case User.get_by_login(login) do
      {:ok, user} ->
        {:ok, token} = Token.find_token_for_user(user, token_desc)
        {user, token}

      {:error, :not_found} ->
        user = User.create!(%{name: name, login: login})
        Group.create!(%{name: name, admin: false, users: [user.id]})
        token = Token.create_api_token(user, :api, token_desc)
        {user, token}
    end
  end

  defp request_analysis_for_data_source(data_source) do
    {_, token} = find_or_create_explorer_creds()

    DataSource.tables(data_source)
    |> Enum.filter(fn %{"columns" => columns} -> Enum.any?(columns, fn %{"user_id" => uid} -> uid end) end)
    |> Enum.each(&request_analysis_for_table(data_source, &1, token))
  end

  defp request_analysis_for_table(data_source, %{"id" => table_name, "columns" => columns}, token) do
    columns
    |> Enum.reject(fn %{"isolated" => isolating?, "user_id" => uid?} -> isolating? || uid? end)
    |> Enum.each(fn %{"name" => column_name} ->
      request_analysis_for_column(data_source, table_name, column_name, token)
    end)
  end

  defp request_analysis_for_column(data_source, table_name, column_name, token) do
    body =
      %{
        "ApiKey" => token,
        "DataSourceName" => data_source.name,
        "TableName" => table_name,
        "ColumnName" => column_name
      }
      |> Jason.encode!()

    with {:ok, %HTTPoison.Response{status_code: 200, body: response}} <-
           HTTPoison.post(base_url() <> "/explore", body, [{"Content-Type", "application/json"}]),
         {:ok, decoded} <- Jason.decode(response),
         {:ok, analysis} <-
           %ExplorerAnalysis{data_source: data_source, table_name: table_name, column: column_name}
           |> ExplorerAnalysis.from_result_json(decoded)
           |> Air.Repo.insert() do
      {:ok, analysis}
    else
      {:error, _err} ->
        %ExplorerAnalysis{
          data_source: data_source,
          table_name: table_name,
          column: column_name,
          status: :error
        }
        |> Air.Repo.insert()

        :error
    end
  end

  defp poll_for_update(explorer_analysis) do
    with {:ok, %HTTPoison.Response{status_code: 200, body: response}} <-
           HTTPoison.get("#{base_url()}/result/#{explorer_analysis.job_id}"),
         {:ok, decoded} <- Jason.decode(response),
         {:ok, analysis} <-
           explorer_analysis
           |> ExplorerAnalysis.from_result_json(decoded)
           |> Air.Repo.update() do
      {:ok, analysis}
    else
      {:ok, %HTTPoison.Response{status_code: 404}} ->
        handle_retry(explorer_analysis)

      {:ok, %HTTPoison.Response{status_code: 500}} ->
        handle_poll_error(explorer_analysis, "Something went wrong in Diffix Explorer")

      {:error, err} ->
        handle_poll_error(explorer_analysis, err)
    end
  end

  defp handle_retry(explorer_analysis) do
    explorer_analysis = Repo.preload(explorer_analysis, :data_source)

    Logger.warn(
      "Explorer returned 404 for previously created job (#{explorer_analysis.data_source.name}/#{
        explorer_analysis.table_name
      }/%{explorer_analysis.column}). Retrying."
    )

    {_, token} = find_or_create_explorer_creds()

    Repo.delete!(explorer_analysis)

    request_analysis_for_column(
      explorer_analysis.data_source,
      explorer_analysis.table_name,
      explorer_analysis.column,
      token
    )

    {:error, "Job deleted in Explorer"}
  end

  defp handle_poll_error(explorer_analysis, error) do
    explorer_analysis
    |> ExplorerAnalysis.changeset(%{status: :error})
    |> Air.Repo.update()

    {:error, error}
  end

  defp base_url(), do: config!("url")

  defp config!(key), do: Map.fetch!(Aircloak.DeployConfig.fetch!("explorer"), key)
end

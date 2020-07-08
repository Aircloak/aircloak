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

  @doc "Returns counts of tables in various stages grouped by data source."
  @spec statistics :: [
          %{
            complete: integer,
            processing: integer,
            error: integer,
            total: integer,
            id: integer,
            name: String.t(),
            description: String.t()
          }
        ]
  def statistics() do
    from(ea in ExplorerAnalysis,
      right_join: ds in DataSource,
      on: ea.data_source_id == ds.id,
      join: dsg in "data_sources_groups",
      on: dsg.data_source_id == ds.id,
      join: g in Air.Schemas.Group,
      on: g.id == dsg.group_id,
      where: g.name == "Diffix Explorer" and g.system,
      select: %{
        complete: count(ea.status in ["complete"] or nil),
        processing: count(ea.status in ["new", "processing"] or nil),
        error: count(ea.status in ["error"] or nil),
        total: count(ea.id),
        id: ds.id,
        name: ds.name,
        description: ds.description
      },
      group_by: ds.id
    )
    |> Repo.all()
  end

  @doc "Is diffix explorer integration enabled for this datasource?"
  @spec data_source_enabled?(DataSource.t()) :: boolean
  def data_source_enabled?(ds) do
    if enabled?() do
      {user, _} = find_or_create_explorer_creds()
      match?({:ok, _}, Air.Service.DataSource.fetch_as_user({:id, ds.id}, user))
    else
      false
    end
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

  @doc "Requests new analyses for datasource."
  @spec reanalyze_datasource(Air.Schemas.DataSource.t()) :: :ok
  def reanalyze_datasource(ds) do
    GenServer.cast(__MODULE__, {:request_analysis_for_data_source, ds})
  end

  @doc """
  Modifies the data source membership of the Diffix Explorer group.
  It then deletes all analysis results belonging to data sources that Diffix Explorer no longer has access to.
  Finally it will request analysis results of all groups that have been added to it.
  """
  @spec change_permitted_data_sources(map) :: {:ok, Group.t()} | {:error, Ecto.Changeset.t()}
  def change_permitted_data_sources(params) do
    old_group = group()
    old_data_source_ids = Enum.map(old_group.data_sources, & &1.id)

    case Air.Service.Group.update(old_group, params) do
      {:ok, new_group} ->
        new_group.data_sources
        |> delete_all_unauthorized()
        |> Enum.reject(fn ds -> Enum.member?(old_data_source_ids, ds.id) end)
        |> Enum.each(&reanalyze_datasource/1)

        {:ok, new_group}

      {:error, changeset} ->
        {:error, changeset}
    end
  end

  @doc "Creates the Diffix Explorer user and group unless they exist already."
  @spec setup_credentials_if_required() :: :ok
  def setup_credentials_if_required() do
    if enabled?(), do: find_or_create_explorer_creds()
    :ok
  end

  @doc "Returns the explorer user"
  @spec user() :: Air.Schemas.User.t()
  def user() do
    {user, _} = find_or_create_explorer_creds()
    user
  end

  @doc "Returns the group which authorizes Diffix Explorer"
  @spec group() :: Air.Schemas.Group.t()
  def group() do
    group = Enum.find(user().groups, fn group -> group.name == "Diffix Explorer" && group.system end)
    Group.load(group.id)
  end

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(_) do
    if enabled?() do
      Process.send_after(self(), :poll, @poll_interval)
      {:ok, true}
    else
      {:ok, false}
    end
  end

  @impl GenServer
  def handle_cast({:request_analysis_for_data_source, data_source}, poll_in_progress?) do
    request_analysis_for_data_source(data_source)
    unless poll_in_progress?, do: Process.send_after(self(), :poll, @poll_interval)
    {:noreply, true}
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

  defp delete_all_unauthorized(current_datasources) do
    Repo.delete_all(
      from(ea in ExplorerAnalysis,
        where: not (ea.data_source_id in ^Enum.map(current_datasources, & &1.id))
      )
    )

    current_datasources
  end

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
        user = User.create!(%{name: name, login: login, system: true})
        Group.create!(%{name: name, admin: false, users: [user.id], system: true})
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
    body =
      %{
        "ApiUrl" => AirWeb.Endpoint.url() <> "/api/",
        "ApiKey" => token,
        "DataSource" => data_source.name,
        "Table" => table_name,
        "Columns" =>
          columns
          |> Enum.reject(fn %{"isolated" => isolating?, "user_id" => uid?} -> isolating? || uid? end)
          |> Enum.map(fn %{"name" => column_name} ->
            column_name
          end)
      }
      |> Jason.encode!()

    with {:ok, %HTTPoison.Response{status_code: 200, body: response}} <-
           HTTPoison.post(base_url() <> "/explore", body, [{"Content-Type", "application/json"}]),
         {:ok, decoded} <- Jason.decode(response),
         {:ok, analysis} <- upsert_analysis(data_source, table_name, result: decoded) do
      {:ok, analysis}
    else
      {:ok, %HTTPoison.Response{status_code: status_code, body: err}} when status_code >= 400 ->
        Logger.error(
          "Explorer encountered an unexpected error (HTTP: #{status_code}) when requesting an analysis for #{
            data_source.name
          }/#{table_name}: #{err}"
        )

        {:ok, _} =
          upsert_analysis(data_source, table_name,
            status: :error,
            errors: ["HTTP #{status_code} during initial request. #{err}"]
          )

        :error

      err ->
        Logger.error(
          "Explorer encountered an unexpected error when requesting an analysis for #{data_source.name}/#{table_name}: #{
            inspect(err)
          }"
        )

        {:ok, _} = upsert_analysis(data_source, table_name, status: :error, errors: [inspect(err)])

        :error
    end
  end

  defp upsert_analysis(data_source, table_name, opts) do
    changeset =
      case Repo.one(
             from(ea in ExplorerAnalysis,
               where: ea.data_source_id == ^data_source.id and ea.table_name == ^table_name
             )
           ) do
        nil -> %ExplorerAnalysis{data_source: data_source, table_name: table_name}
        analysis -> analysis
      end

    modified =
      if Keyword.has_key?(opts, :result) do
        ExplorerAnalysis.from_result_json(changeset, Keyword.get(opts, :result))
      else
        ExplorerAnalysis.changeset(changeset, Enum.into(opts, %{}))
      end

    Repo.insert_or_update(modified)
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

      {:ok, %HTTPoison.Response{status_code: 500, body: body}} ->
        handle_poll_error(
          explorer_analysis,
          "Something went wrong in Diffix Explorer (HTTP 500) when polling: " <> body
        )

      err ->
        handle_poll_error(explorer_analysis, inspect(err))
    end
  end

  defp handle_retry(explorer_analysis) do
    explorer_analysis = Repo.preload(explorer_analysis, :data_source)

    Logger.warn(
      "Explorer returned 404 for previously created job (#{explorer_analysis.data_source.name}/#{
        explorer_analysis.table_name
      }). Retrying."
    )

    {_, token} = find_or_create_explorer_creds()

    Repo.delete!(explorer_analysis)

    request_analysis_for_table(
      explorer_analysis.data_source,
      explorer_analysis.table_name,
      token
    )

    {:error, "Job deleted in Explorer"}
  end

  defp handle_poll_error(explorer_analysis, error) do
    explorer_analysis = Repo.preload(explorer_analysis, :data_source)

    Logger.error(
      "Polling for results for #{explorer_analysis.data_source.name}/#{explorer_analysis.table_name} errored with #{
        inspect(error)
      }."
    )

    explorer_analysis
    |> ExplorerAnalysis.changeset(%{status: :error, errors: [error]})
    |> Air.Repo.update()

    {:error, error}
  end

  defp base_url(), do: config!("url")

  defp config!(key), do: Map.fetch!(Aircloak.DeployConfig.fetch!("explorer"), key)
end

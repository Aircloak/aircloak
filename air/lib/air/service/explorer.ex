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
    from(explorer_analysis in ExplorerAnalysis,
      right_join: data_source in DataSource,
      on: explorer_analysis.data_source_id == data_source.id,
      join: data_source_groups in "data_sources_groups",
      on: data_source_groups.data_source_id == data_source.id,
      join: group in Air.Schemas.Group,
      on: group.id == data_source_groups.group_id,
      where: group.name == "Diffix Explorer" and group.system,
      select: %{
        complete: count(explorer_analysis.status in ["complete"] or nil),
        processing: count(explorer_analysis.status in ["new", "processing"] or nil),
        error: count(explorer_analysis.status in ["error"] or nil),
        total: count(explorer_analysis.id),
        id: data_source.id,
        name: data_source.name,
        description: data_source.description
      },
      group_by: data_source.id
    )
    |> Repo.all()
  end

  @doc "Is diffix explorer integration enabled for this datasource?"
  @spec data_source_enabled?(DataSource.t()) :: boolean
  def data_source_enabled?(data_source) do
    if enabled?() do
      {user, _} = find_or_create_explorer_creds()
      match?({:ok, _}, Air.Service.DataSource.fetch_as_user({:id, data_source.id}, user))
    else
      false
    end
  end

  @doc "Returns analysis results for a particular data source"
  @spec results_for_datasource(DataSource.t()) :: [ExplorerAnalysis.t()]
  def results_for_datasource(data_source),
    do:
      Repo.all(
        from(explorer_analysis in ExplorerAnalysis,
          where: explorer_analysis.data_source_id == ^data_source.id,
          preload: [:data_source]
        )
      )

  @doc "Requests new analyses for datasource."
  @spec reanalyze_datasource(Air.Schemas.DataSource.t()) :: :ok
  def reanalyze_datasource(data_source),
    do: Enum.each(results_for_datasource(data_source), &GenServer.cast(__MODULE__, {:request_analysis, &1}))

  @doc "Returns a list of tables that are elligible for analysis for a particular datasource"
  @spec elligible_tables_for_datasource(Air.Schemas.DataSource.t()) :: [String.t()]
  def elligible_tables_for_datasource(data_source) do
    DataSource.tables(data_source)
    |> Enum.filter(fn table -> Enum.any?(table["columns"], & &1["user_id"]) end)
    |> Enum.map(fn %{"id" => table_name} -> table_name end)
  end

  @doc "Removes results for tables which no longer exist in the data source."
  @spec data_source_updated(Air.Schemas.DataSource.t()) :: :ok
  def data_source_updated(data_source) do
    tables =
      DataSource.tables(data_source)
      |> Enum.map(fn %{"id" => name} -> name end)

    from(explorer_analysis in ExplorerAnalysis,
      where:
        explorer_analysis.data_source_id == ^data_source.id and
          explorer_analysis.table_name not in ^tables
    )
    |> Repo.delete_all()

    :ok
  end

  @doc """
  Modifies the data source membership of the Diffix Explorer group.
  It then deletes all analysis results belonging to data sources that Diffix Explorer no longer has access to.
  Finally it will request analysis results of all tables of all groups that have been added to it.
  """
  @spec change_permitted_data_sources(map) :: {:ok, Group.t()} | {:error, Ecto.Changeset.t()}
  def change_permitted_data_sources(params) do
    old_group = group()

    tables_by_datasource =
      Enum.reduce(params["tables"], %{}, fn input, acc ->
        with true <- is_map(input),
             [{data_source_id_str, table_name}] <- Map.to_list(input),
             {data_source_id, _remainder} <- Integer.parse(data_source_id_str) do
          Map.update(acc, data_source_id, [table_name], &[table_name | &1])
        else
          _ -> acc
        end
      end)

    case Air.Service.Group.update(old_group, params) do
      {:ok, new_group} ->
        new_group.data_sources
        |> Enum.map(fn data_source -> {data_source, tables_by_datasource[data_source.id] || []} end)
        |> delete_all_unauthorized()
        |> reject_all_unchanged()
        |> Enum.each(fn {data_source, tables} ->
          Enum.each(tables, fn table ->
            GenServer.cast(__MODULE__, {:request_analysis, create_placeholder_result(data_source, table)})
          end)
        end)

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
    user = Repo.preload(user(), :groups)

    user.groups
    |> Enum.find(&(&1.name == "Diffix Explorer" && &1.system))
    |> Repo.preload(:data_sources)
  end

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(_) do
    if enabled?() do
      {:ok, schedule_poll()}
    else
      {:ok, false}
    end
  end

  @impl GenServer
  def handle_cast({:request_analysis, analysis}, poll_in_progress?) do
    request_analysis(analysis)
    {:noreply, poll_unless_already_pending_poll(poll_in_progress?)}
  end

  @impl GenServer
  def handle_info(:poll, _state) do
    poll_results =
      pending_analyses_query()
      |> Repo.all()
      |> Enum.map(&poll_analysis/1)

    if Repo.exists?(pending_analyses_query()) do
      poll_interval =
        if Enum.any?(poll_results, &timed_out?/1) do
          @poll_interval * 10
        else
          @poll_interval
        end

      {:noreply, schedule_poll(poll_interval)}
    else
      {:noreply, false}
    end
  end

  def handle_info({:ssl_closed, {:sslsocket, {:gen_tcp, _port, :tls_connection, :undefined}, _pids}}, state) do
    Logger.warn(fn -> "The SSL connection was unexpectedly terminated by Explorer." end)
    {:noreply, poll_unless_already_pending_poll(state)}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp timed_out?({:error, :timeout}), do: true
  defp timed_out?(_), do: false

  defp schedule_poll(poll_interval \\ @poll_interval), do: poll_unless_already_pending_poll(false, poll_interval)

  defp poll_unless_already_pending_poll(poll_in_progress?, poll_interval \\ @poll_interval) do
    if not poll_in_progress? do
      Process.send_after(self(), :poll, poll_interval)
    end

    true
  end

  defp create_placeholder_result(data_source, table) do
    Repo.insert!(%ExplorerAnalysis{data_source: data_source, table_name: table, status: :new})
  end

  defp delete_all_unauthorized(current_datasources) do
    current_datasources
    |> Enum.reduce(ExplorerAnalysis, fn {data_source, tables}, query ->
      if Enum.empty?(tables) do
        query
      else
        where(
          query,
          [explorer_analysis],
          not (explorer_analysis.data_source_id == ^data_source.id and explorer_analysis.table_name in ^tables)
        )
      end
    end)
    |> Repo.delete_all()

    current_datasources
  end

  defp reject_all_unchanged(current_datasources) do
    results = Repo.all(ExplorerAnalysis)

    current_datasources
    |> Enum.map(fn {data_source, tables} ->
      {data_source,
       Enum.reject(tables, fn table ->
         Enum.find(results, false, fn result ->
           result.data_source_id == data_source.id && result.table_name == table
         end)
       end)}
    end)
    |> Enum.reject(fn {_data_source, tables} -> Enum.empty?(tables) end)
  end

  defp pending_analyses_query() do
    from(explorer_analysis in ExplorerAnalysis,
      where: explorer_analysis.status in ["new", "processing"] and not is_nil(explorer_analysis.job_id),
      preload: [:data_source]
    )
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

  defp request_analysis(analysis) do
    {_, token} = find_or_create_explorer_creds()

    %{"columns" => columns} =
      DataSource.tables(analysis.data_source)
      |> Enum.find(fn %{"id" => name} -> name == analysis.table_name end)

    body =
      %{
        "ApiUrl" => AirWeb.Endpoint.url() <> "/api/",
        "ApiKey" => token,
        "DataSource" => analysis.data_source.name,
        "Table" => analysis.table_name,
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
         {:ok, analysis} <- update_analysis(analysis, result: decoded) do
      {:ok, analysis}
    else
      {:ok, %HTTPoison.Response{status_code: status_code, body: err}} when status_code >= 400 ->
        Logger.error(
          "Explorer encountered an unexpected error (HTTP: #{status_code}) when requesting an analysis for #{
            analysis.data_source.name
          }/#{analysis.table_name}: #{err}"
        )

        {:ok, _} =
          update_analysis(analysis,
            status: :error,
            errors: ["HTTP #{status_code} during initial request. #{err}"]
          )

        :error

      err ->
        Logger.error(
          "Explorer encountered an unexpected error when requesting an analysis for #{analysis.data_source.name}/#{
            analysis.table_name
          }: #{inspect(err)}"
        )

        {:ok, _} = update_analysis(analysis, status: :error, errors: [inspect(err)])

        :error
    end
  end

  defp handle_result_changeset(changeset, nil), do: changeset
  defp handle_result_changeset(changeset, results), do: ExplorerAnalysis.from_decoded_result_json(changeset, results)

  defp update_analysis(changeset, opts) do
    changeset
    |> handle_result_changeset(Keyword.get(opts, :result))
    |> ExplorerAnalysis.changeset(
      opts
      |> Keyword.delete(:result)
      |> Enum.into(%{})
    )
    |> Repo.insert_or_update()
  end

  defp poll_analysis(explorer_analysis) do
    case poll_explorer_for_update(explorer_analysis) do
      {:ok, decoded_analysis} ->
        explorer_analysis
        |> ExplorerAnalysis.from_decoded_result_json(decoded_analysis)
        |> Air.Repo.update()

      {:error, :not_found} ->
        Logger.warn(
          "Restarting analysis job (#{explorer_analysis.data_source.name}/#{explorer_analysis.table_name}) due to Diffix Explorer having lost track of it."
        )

        handle_retry(explorer_analysis)

      {:error, {:internal_error, status_code, body}} ->
        handle_poll_error(
          explorer_analysis,
          "Something went wrong in Diffix Explorer (HTTP #{status_code}) when polling: " <> body
        )

        {:error, :explorer_error}

      {:error, :timeout} ->
        # Note: we are explicitly not marking timed out jobs as errored, because this will in turn
        # mark the job as failed, and prevent it from being re-attempted. A timeout is likely just a temporary glitch.
        Logger.warn(fn ->
          "Polling request for explorer job with id #{explorer_analysis.job_id} timed out. Will attempt again later."
        end)

        {:error, :timeout}

      {:error, unexpected_result} ->
        handle_poll_error(explorer_analysis, inspect(unexpected_result))
        {:error, :unexpected_result}
    end
  end

  defp poll_explorer_for_update(explorer_analysis) do
    with {:ok, %HTTPoison.Response{status_code: 200, body: response}} <-
           HTTPoison.get("#{base_url()}/result/#{explorer_analysis.job_id}") do
      Jason.decode(response)
    else
      {:ok, %HTTPoison.Response{status_code: 404}} ->
        {:error, :not_found}

      {:ok, %HTTPoison.Response{status_code: status_code, body: body}} when status_code >= 500 and status_code < 600 ->
        {:error, {:internal_error, status_code, body}}

      {:error, %HTTPoison.Error{reason: :timeout}} ->
        {:error, :timeout}

      err ->
        {:error, err}
    end
  end

  defp handle_retry(explorer_analysis) do
    update_analysis(explorer_analysis, job_id: nil)
    request_analysis(explorer_analysis)
  end

  defp handle_poll_error(explorer_analysis, error) do
    Logger.error(
      "Polling for results for #{explorer_analysis.data_source.name}/#{explorer_analysis.table_name} errored with #{
        inspect(error)
      }."
    )

    explorer_analysis
    |> ExplorerAnalysis.changeset(%{status: :error, errors: [error]})
    |> Air.Repo.update()
  end

  defp base_url(), do: config!("url") <> "/api/v1"

  defp config!(key), do: Map.fetch!(Aircloak.DeployConfig.fetch!("explorer"), key)
end

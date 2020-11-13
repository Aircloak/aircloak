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

  @doc "Returns all data sources along with the some metadata about the tables selected."
  @spec all_data_source_metadata :: [
          %{
            id: integer,
            name: String.t(),
            selected_tables: [String.t()],
            stats: %{
              total: integer,
              complete: integer,
              processing: integer,
              error: integer
            },
            tables: %{
              name: String.t(),
              status: :not_enabled | :new | :processing | :cancelled | :error | :complete
            }
          }
        ]
  def all_data_source_metadata() do
    from(data_source in DataSource,
      left_join: explorer_analysis in ExplorerAnalysis,
      on: explorer_analysis.data_source_id == data_source.id,
      select: %{
        id: data_source.id,
        name: data_source.name,
        tables: data_source.tables,
        analysis_id: explorer_analysis.id,
        table_name: explorer_analysis.table_name,
        analysis_status: explorer_analysis.status,
        soft_delete: explorer_analysis.soft_delete
      }
    )
    |> Repo.all()
    |> Enum.group_by(&{&1.id, &1.name, &1.tables})
    |> Enum.map(fn {{id, name, tables}, selected_tables} ->
      selected_tables = Enum.reject(selected_tables, &is_nil(&1.analysis_id))

      soft_deleted_table_names =
        selected_tables
        |> Enum.filter(& &1.soft_delete)
        |> Enum.map(& &1.table_name)

      selected_table_names =
        selected_tables
        |> Enum.map(& &1.table_name)
        |> Enum.reject(&(&1 in soft_deleted_table_names))
        |> Enum.sort()

      available_table_names =
        case Jason.decode(tables) do
          {:ok, tables} ->
            tables
            |> Enum.filter(fn table -> Enum.any?(table["columns"], & &1["user_id"]) end)
            |> Enum.map(fn %{"id" => table_name} -> table_name end)
            |> Enum.reject(&(&1 in soft_deleted_table_names))
            |> Enum.sort()

          _ ->
            []
        end

      tables_with_state =
        available_table_names
        |> Enum.map(fn table_name ->
          table = Enum.find(selected_tables, %{analysis_status: :not_enabled}, &(&1.table_name == table_name))

          %{
            name: table_name,
            status: table.analysis_status
          }
        end)

      {complete, processing, error} =
        Enum.reduce(selected_tables, {0, 0, 0}, fn table, {complete_acc, processing_acc, error_acc} ->
          case table.analysis_status do
            status when status in [:complete] -> {complete_acc + 1, processing_acc, error_acc}
            status when status in [:new, :processing] -> {complete_acc, processing_acc + 1, error_acc}
            status when status in [:error, :cancelled] -> {complete_acc, processing_acc, error_acc + 1}
            _else -> {complete_acc, processing_acc, error_acc}
          end
        end)

      %{
        id: id,
        name: name,
        selected_tables: selected_table_names,
        tables: tables_with_state,
        stats: %{
          total: length(selected_table_names),
          complete: complete,
          processing: processing,
          error: error
        }
      }
    end)
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

  @doc "Spawns the analysis of a table, whether or not it has previously been analyzed"
  @spec analyze_table(data_source_id :: integer, table_name :: String.t()) :: :ok
  def analyze_table(data_source_id, table_name),
    do: GenServer.cast(__MODULE__, {:analyze_table, data_source_id, table_name})

  @doc "Removes a table from being analyzed."
  @spec disable_table(data_source_id :: integer, table_name :: String.t()) :: :ok
  def disable_table(data_source_id, table_name),
    do: GenServer.cast(__MODULE__, {:disable_table, data_source_id, table_name})

  @doc "Removes results for tables which no longer exist in the data source."
  @spec data_source_updated(Air.Schemas.DataSource.t()) :: :ok
  def data_source_updated(data_source) do
    tables =
      DataSource.tables(data_source)
      |> Enum.map(fn %{"id" => name} -> name end)

    Repo.update_all(
      from(explorer_analysis in ExplorerAnalysis,
        where:
          explorer_analysis.data_source_id == ^data_source.id and
            explorer_analysis.table_name in ^tables
      ),
      set: [soft_delete: false]
    )

    Repo.update_all(
      from(explorer_analysis in ExplorerAnalysis,
        where:
          explorer_analysis.data_source_id == ^data_source.id and
            explorer_analysis.table_name not in ^tables
      ),
      set: [soft_delete: true]
    )

    broadcast_changes()

    :ok
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

  def handle_cast({:analyze_table, data_source_id, table_name}, poll_in_progress?) do
    existing_analysis =
      Repo.one(
        from(explorer_analysis in ExplorerAnalysis,
          where: explorer_analysis.data_source_id == ^data_source_id and explorer_analysis.table_name == ^table_name,
          preload: [:data_source]
        )
      )

    if is_nil(existing_analysis) do
      {:ok, data_source} = get_data_source_and_grant_access_if_needed(data_source_id)
      # We create an analysis placeholder. This will be the only place in the service creating analysis.
      # That way we can assume that if one doesn't exist, then it shouldn't exist either.
      %ExplorerAnalysis{data_source: data_source}
      |> ExplorerAnalysis.changeset(%{
        table_name: table_name,
        status: :new
      })
      |> Repo.insert!()
      |> Repo.preload(:data_source)
      |> request_analysis()
    else
      unless existing_analysis.soft_delete do
        existing_analysis
        |> ExplorerAnalysis.changeset(%{status: :new})
        |> Repo.update!()
        |> request_analysis()
      end
    end

    broadcast_changes()
    {:noreply, poll_unless_already_pending_poll(poll_in_progress?)}
  end

  def handle_cast({:disable_table, data_source_id, table_name}, poll_in_progress?) do
    analysis =
      Repo.one(
        from(explorer_analysis in ExplorerAnalysis,
          where: explorer_analysis.data_source_id == ^data_source_id and explorer_analysis.table_name == ^table_name,
          preload: :data_source
        )
      )

    unless is_nil(analysis) do
      if analysis.status in [:new, :processing], do: cancel_analysis(analysis)
      Repo.delete!(analysis)
      broadcast_changes()
    end

    {:noreply, poll_in_progress?}
  end

  @impl GenServer
  def handle_info(:poll, _state) do
    pending_analyses_query()
    |> Repo.all()
    |> Enum.each(&poll_analysis/1)

    if Repo.exists?(pending_analyses_query()) do
      {:noreply, schedule_poll()}
    else
      {:noreply, false}
    end
  end

  def handle_info({:ssl_closed, {:sslsocket, {:gen_tcp, _port, :tls_connection, :undefined}, _pids}}, state) do
    Logger.info("The SSL connection was unexpectedly terminated by Explorer.")
    {:noreply, poll_unless_already_pending_poll(state)}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp get_data_source_and_grant_access_if_needed(data_source_id) do
    {user, _} = find_or_create_explorer_creds()

    case Air.Service.DataSource.fetch_as_user({:id, data_source_id}, user) do
      {:ok, data_source} ->
        {:ok, data_source}

      {:error, :unauthorized} ->
        data_source = Repo.get_by(Air.Schemas.DataSource, id: data_source_id)
        group = group()
        new_data_sources = [data_source_id | group.data_sources |> Enum.map(& &1.id)]
        Air.Service.Group.update!(group, %{data_sources: new_data_sources})
        {:ok, data_source}
    end
  end

  defp schedule_poll(), do: poll_unless_already_pending_poll(false)

  defp poll_unless_already_pending_poll(poll_in_progress?) do
    if not poll_in_progress? do
      Process.send_after(self(), :poll, @poll_interval)
    end

    true
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
    set_status_to_processing(analysis)

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
          |> Enum.reject(&unanalyzable_column?/1)
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
            analysis_name(analysis)
          }: #{err}"
        )

        {:ok, _} =
          update_analysis(analysis,
            status: :error,
            errors: ["HTTP #{status_code} during initial request. #{err}"]
          )

        :error

      err ->
        Logger.error(
          "Explorer encountered an unexpected error when requesting an analysis for #{analysis_name(analysis)}: #{
            inspect(err)
          }"
        )

        {:ok, _} = update_analysis(analysis, status: :error, errors: [inspect(err)])

        :error
    end
  end

  defp cancel_analysis(analysis) do
    case HTTPoison.get(base_url() <> "/cancel/#{analysis.job_id}") do
      {:ok, %HTTPoison.Response{status_code: 200, body: _response}} ->
        Logger.info("Cancelled analysis of #{analysis_name(analysis)}")

      _ ->
        Logger.warn("Ignoring failed attempt a cancelling analysis of #{analysis_name(analysis)}.")
    end
  end

  defp unanalyzable_column?(column),
    do:
      Map.get(column, "isolated", true) || Map.get(column, "user_id", true) ||
        Map.get(column, "access", "unselectable") == "unselectable"

  defp handle_result_changeset(changeset, nil), do: changeset
  defp handle_result_changeset(changeset, results), do: ExplorerAnalysis.from_decoded_result_json(changeset, results)

  defp set_status_to_processing(analysis) do
    {:ok, analysis} = Repo.update(ExplorerAnalysis.changeset(analysis, %{status: :processing}))
    analysis
  end

  defp update_analysis(changeset, opts) do
    result =
      changeset
      |> handle_result_changeset(Keyword.get(opts, :result))
      |> ExplorerAnalysis.changeset(
        opts
        |> Keyword.delete(:result)
        |> Enum.into(%{})
      )
      |> Repo.update()

    broadcast_changes()
    result
  end

  defp poll_analysis(explorer_analysis) do
    case poll_explorer_for_update(explorer_analysis) do
      {:ok, decoded_analysis} ->
        explorer_analysis
        |> ExplorerAnalysis.from_decoded_result_json(decoded_analysis)
        |> Air.Repo.update()

        broadcast_changes()

      {:error, :not_found} ->
        Logger.warn(
          "Restarting analysis job (#{explorer_analysis.data_source.name}/#{explorer_analysis.table_name}) due to Diffix Explorer having lost track of it."
        )

        handle_retry(explorer_analysis)

      {:error, {:client_error, status_code, body}} ->
        handle_poll_error(
          explorer_analysis,
          "Diffix Explorer rejected the request (HTTP #{status_code}) when polling: " <> body
        )

        {:error, :air_error}

      {:error, {:internal_error, status_code, body}} ->
        handle_poll_error(
          explorer_analysis,
          "Something went wrong in Diffix Explorer (HTTP #{status_code}) when polling: " <> body
        )

        {:error, :explorer_error}

      {:error, :timeout} ->
        # Note: we are explicitly not marking timed out jobs as errored, because this will in turn
        # mark the job as failed, and prevent it from being re-attempted. A timeout is likely just a temporary glitch.
        Logger.warn(
          "Polling request for explorer job with id #{explorer_analysis.job_id} timed out. Will attempt again later."
        )

        {:error, :timeout}

      {:error, unexpected_result} ->
        handle_poll_error(explorer_analysis, inspect(unexpected_result))
        {:error, :unexpected_result}
    end
  end

  defp poll_explorer_for_update(explorer_analysis) do
    case HTTPoison.get("#{base_url()}/result/#{explorer_analysis.job_id}") do
      {:ok, %HTTPoison.Response{status_code: 200, body: response}} ->
        Jason.decode(response)

      {:error, %HTTPoison.Error{reason: :timeout}} ->
        {:error, :timeout}

      {:ok, %HTTPoison.Response{status_code: 404}} ->
        {:error, :not_found}

      {:ok, %HTTPoison.Response{status_code: status_code, body: body}} when status_code >= 400 and status_code < 500 ->
        {:error, {:client_error, status_code, body}}

      {:ok, %HTTPoison.Response{status_code: status_code, body: body}} when status_code >= 500 and status_code < 600 ->
        {:error, {:internal_error, status_code, body}}

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

    broadcast_changes()
  end

  defp analysis_name(analysis), do: analysis.data_source.name <> "/" <> analysis.table_name

  defp base_url(), do: config!("url") <> "/api/v1"

  defp config!(key), do: Map.fetch!(Aircloak.DeployConfig.fetch!("explorer"), key)

  defp broadcast_changes(), do: AirWeb.Endpoint.broadcast!("explorer", "analysis_updated", %{})
end

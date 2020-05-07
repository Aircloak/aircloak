defmodule Air.Service.Explorer do
  @moduledoc """
  Implements a service for interacting with a Diffix Explorer serice.
  """
  # use GenServer
  alias Air.Repo
  alias Air.Schemas.{DataSource, ExplorerAnalysis}
  require Aircloak.DeployConfig
  import Ecto.Query
  import Aircloak, only: [in_env: 1]

  @poll_interval in_env(test: 0, else: :timer.seconds(5))

  @doc "Is the Diffix Explorer integration enabled? Other methods should only be called if this returns `true`."
  @spec enabled? :: boolean
  def enabled?() do
    case Aircloak.DeployConfig.fetch("explorer") do
      {:ok, _} -> true
      :error -> false
    end
  end

  @doc "Is diffix explorer integration enabled for this datasource?"
  @spec data_source_supported?(DataSource.t()) :: boolean
  def data_source_supported?(ds), do: ds.name in data_source_names()

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

  def reanalyze_datasource(ds) do
    Repo.delete_all(
      from(ea in ExplorerAnalysis,
        where: ea.data_source_id == ^ds.id
      )
    )

    request_analysis_for_data_source(ds)
  end

  @doc "Returns all analysis results."
  @spec all :: [ExplorerAnalysis.t()]
  def all(), do: Repo.all(from(ExplorerAnalysis, preload: [:data_source]))

  @doc "Kicks the system off. Will issue a number of requests to the Diffix Explorer system."
  @spec begin_analyses :: :ok
  def begin_analyses() do
    from(ds in DataSource, where: ds.name in ^data_source_names())
    |> Repo.all()
    |> Enum.each(&request_analysis_for_data_source/1)
  end

  def reanalyze() do
    Repo.delete_all(from(ExplorerAnalysis))
    begin_analyses()
    poll_for_updates()
    all()
  end

  @doc "Will poll all pending jobs to obtain results."
  @spec poll_for_updates :: :ok
  def poll_for_updates() do
    older_than_limit = NaiveDateTime.utc_now() |> NaiveDateTime.add(-@poll_interval, :millisecond)

    pending_analyses =
      from(ea in ExplorerAnalysis,
        where: ea.status in ["new", "processing"] and ea.last_request <= ^older_than_limit
      )
      |> Repo.all()

    Enum.each(pending_analyses, &poll_for_update/1)

    if Repo.exists?(from(ea in ExplorerAnalysis, where: ea.status in ["new", "processing"])) do
      Process.sleep(@poll_interval)
      poll_for_updates()
    end

    :ok
  end

  defp request_analysis_for_data_source(data_source) do
    DataSource.tables(data_source)
    |> Enum.filter(fn %{"columns" => columns} -> Enum.any?(columns, fn %{"user_id" => uid} -> uid end) end)
    |> Enum.each(&request_analysis_for_table(data_source, &1))
  end

  defp request_analysis_for_table(data_source, %{"id" => table_name, "columns" => columns}) do
    Enum.each(columns, fn %{"name" => column_name} ->
      request_analysis_for_column(data_source, table_name, column_name)
    end)
  end

  defp request_analysis_for_column(data_source, table_name, column_name) do
    body =
      %{
        "ApiKey" => api_key(),
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
      {:error, err} ->
        IO.inspect(err, label: "Error")
        :error
    end
  end

  defp poll_for_update(explorer_analysis) do
    IO.inspect(explorer_analysis, label: "polling")

    with {:ok, %HTTPoison.Response{status_code: 200, body: response}} <-
           HTTPoison.get("#{base_url()}/result/#{explorer_analysis.job_id}"),
         {:ok, decoded} <- Jason.decode(response),
         {:ok, analysis} <-
           explorer_analysis
           |> ExplorerAnalysis.from_result_json(decoded)
           |> Air.Repo.update() do
      IO.puts("#{explorer_analysis.column}: #{decoded["status"]}")
      {:ok, analysis}
    else
      {:ok, %HTTPoison.Response{status_code: 404}} ->
        # Diffix Explorer doesn't know about this job. In that case, we delete it from the DB
        # and try again.
        explorer_analysis = Repo.preload(explorer_analysis, :data_source)
        Repo.delete!(explorer_analysis)

        request_analysis_for_column(
          explorer_analysis.data_source,
          explorer_analysis.table_name,
          explorer_analysis.column
        )

        IO.puts("Error, got 404")
        {:error, explorer_analysis}

      {:ok, %HTTPoison.Response{status_code: 500}} ->
        IO.puts("Error: got 500")

        explorer_analysis
        |> ExplorerAnalysis.changeset(%{status: :error})
        |> Air.Repo.update()

        {:error, "Something went wrong in Diffix Explorer", explorer_analysis}

      {:error, err} ->
        IO.inspect(err, label: "Error")
        :error
    end
  end

  defp data_source_names(), do: config!("data_sources")

  defp base_url(), do: IO.inspect(config!("url"), label: "Base")
  defp api_key(), do: config!("api_key")

  defp config!(key), do: Map.fetch!(Aircloak.DeployConfig.fetch!("explorer"), key)
end

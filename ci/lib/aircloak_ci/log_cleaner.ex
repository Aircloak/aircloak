defmodule AircloakCI.LogCleaner do
  @moduledoc "Process which cleans up old logs for non-existing branches and PRs."

  use GenServer, start: {__MODULE__, :start_link, []}
  require Logger

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @impl GenServer
  def init(nil) do
    AircloakCI.RepoDataProvider.subscribe()
    {:ok, nil}
  end

  @impl GenServer
  def handle_info({:repo_data, repo_data}, state) do
    clean_logs(repo_data)
    {:noreply, state}
  end

  def handle_info(message, state), do: super(message, state)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp clean_logs(repo_data) do
    valid_log_names =
      repo_data.pull_requests
      |> Stream.map(&"pr-#{&1.number}")
      |> Stream.concat(Stream.map(repo_data.branches, &String.replace(&1.name, "/", "-")))
      |> MapSet.new()

    with {:ok, folders} <- File.ls(AircloakCI.LocalProject.logs_folder()) do
      folders
      |> Stream.reject(&Enum.member?(valid_log_names, &1))
      |> Stream.map(&Path.join(AircloakCI.LocalProject.logs_folder(), &1))
      |> Stream.filter(&(age_in_seconds(&1) > days_to_seconds(30)))
      |> Enum.each(&remove_log_folder/1)
    end
  end

  defp age_in_seconds(path) do
    case File.lstat(path) do
      {:ok, stat} ->
        mtime = NaiveDateTime.from_erl!(stat.mtime)
        NaiveDateTime.diff(NaiveDateTime.utc_now(), mtime, :second)

      _ ->
        0
    end
  end

  defp days_to_seconds(days),
    do: System.convert_time_unit(days * :timer.hours(24), :millisecond, :second)

  defp remove_log_folder(path) do
    case File.rm_rf(path) do
      {:ok, _} -> Logger.info("removed logs for `#{Path.basename(path)}`")
      _ -> :ok
    end
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(), do: GenServer.start_link(__MODULE__, nil, name: __MODULE__)
end

defmodule Cloak.DataSource.PostgrexAutoRepair do
  @moduledoc """
  Process which detects known inconsistency in Postgrex and auto-repairs the system.

  For full details, see https://github.com/elixir-ecto/postgrex/issues/362
  The gist of the issue is that postgrex ends up in a corrupt state where it caches dead processes. The consequence is
  that all attempts to connect to corresponding data source will hang (`Postgrex.start_link` never returns).

  Since it's not clear what causes the issue, nor how to reproduce it, we currently use this hacky workaround.
  This process relies on postgrex internals to detect the corrupt state. If this state perists (a couple of consecutive
  checks fail), the type manager process is restarted.
  """
  require Logger

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp loop(state) do
    :timer.sleep(state.check_interval)

    state
    |> analyze_postgrex_liveness()
    |> loop()
  end

  defp initial_state(), do: %{check_interval: :timer.seconds(10), allowed_consecutive_failed_checks: 6}

  defp analyze_postgrex_liveness(state) do
    if inconsistent_postgrex_state?() do
      state.allowed_consecutive_failed_checks
      |> update_in(&(&1 - 1))
      |> handle_failed_check()
    else
      initial_state()
    end
  catch
    type, error ->
      Logger.error(Exception.format(type, error))
      state
  end

  defp handle_failed_check(%{allowed_consecutive_failed_checks: 0}) do
    Logger.error("Too many consecutive postgrex checks failed, restarting postgrex type manager")

    # We only restart the type manager process. This will have the least possible impact on the running queries.
    Process.exit(Process.whereis(Postgrex.TypeManager), :kill)

    initial_state()
  end

  defp handle_failed_check(state), do: state

  defp inconsistent_postgrex_state?() do
    # Hacky detection of the situation described in https://github.com/elixir-ecto/postgrex/issues/362
    {keys, _} = :sys.get_state(Postgrex.TypeManager)
    type_managers = Enum.map(keys, fn {_, pid} -> pid end)
    dead_type_managers = Enum.reject(type_managers, &Process.alive?/1)
    not Enum.empty?(dead_type_managers)
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_) do
    %{id: __MODULE__, start: {__MODULE__, :start_link, []}}
  end

  @doc false
  def start_link(), do: Task.start_link(fn -> loop(initial_state()) end)
end

defmodule AircloakCI.TestExec do
  @moduledoc false

  def start_link() do
    Agent.start_link(fn -> :ets.new(__MODULE__, [:named_table, :public]) end)
  end

  def child_spec(_), do: Supervisor.child_spec(Agent, %{id: __MODULE__, start: {__MODULE__, :start_link, []}})

  def clear_patterns(), do: :ets.delete_all_objects(__MODULE__)

  def add_exec_handler(matcher), do: :ets.insert(__MODULE__, {:erlang.unique_integer(), matcher})

  def run_link(cmd, _opts) do
    {:ok, pid} = Task.start_link(fn -> simulate_command(cmd) end)
    {:ok, pid, nil}
  end

  def stop(_pid) do
    :ok
  end

  defp transfer_component(component, target_folder) do
    target_path = Path.expand(Path.join(target_folder, "#{component}/ci"))
    File.mkdir_p!(target_path)
    File.cp_r!("../#{component}/ci", "#{target_path}")
  end

  defp simulate_command(cmd) do
    :ets.tab2list(__MODULE__)
    |> Enum.sort()
    |> Enum.concat([{nil, &default_command_handler/1}])
    |> Stream.map(fn {_, handler} -> handler.(cmd) end)
    |> Stream.reject(&is_nil/1)
    |> Enum.take(1)
  end

  defp default_command_handler(cmd) do
    case Regex.named_captures(
           ~r[^git clone git@github.com:.* (?<target>\.test_data/.*)$],
           to_string(cmd)
         ) do
      nil ->
        :ok

      %{"target" => target_folder} ->
        AircloakCI.LocalProject.for_local("..")
        |> AircloakCI.LocalProject.changed_components()
        |> Enum.each(&transfer_component(&1, target_folder))
    end
  end
end

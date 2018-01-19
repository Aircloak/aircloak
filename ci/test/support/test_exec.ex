defmodule AircloakCI.TestExec do
  @moduledoc false

  def run_link(cmd, _opts) do
    case Regex.named_captures(~r[^git clone git@github.com:.* (?<target>\.test_data/.*)$], to_string(cmd)) do
      nil -> :ok

      %{"target" => target_folder} ->
        AircloakCI.LocalProject.for_local("..")
        |> AircloakCI.LocalProject.components()
        |> Enum.each(&transfer_component(&1, target_folder))
    end

    {:ok, pid} = Task.start_link(fn -> :ok end)
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
end

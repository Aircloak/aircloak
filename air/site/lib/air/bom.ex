defmodule Air.BOM do
  def start_link, do: Agent.start_link(&read_bom/0, name: __MODULE__)

  def get(f \\ &(&1)), do: Agent.get(__MODULE__, f)

  defp read_bom do
    Application.app_dir(:air)
    |> Path.join(bom_path)
    |> File.read!()
    |> Poison.decode!()
  end

  defp bom_path do
    Application.get_env(:air, Air.BOM)
    |> Keyword.fetch!(:location)
  end
end

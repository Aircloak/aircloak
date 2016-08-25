defmodule Air.BOM do
  def start_link, do: Agent.start_link(&read_bom/0, name: __MODULE__)

  def get(f \\ &(&1)), do: Agent.get(__MODULE__, f)

  defp read_bom do
    Application.get_env(:air, Air.BOM)
    |> Keyword.fetch!(:location)
    |> File.read!()
    |> Poison.decode!()
  end
end

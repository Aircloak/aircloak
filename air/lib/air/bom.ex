defmodule Air.BOM do
  @moduledoc """
  An Agent keeping the data about open source dependencies of the project.  See priv/bom.json.example for how
  the data is structured.
  """


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc false
  @spec start_link :: Agent.on_start
  def start_link, do: Agent.start_link(&read_bom/0, name: __MODULE__)

  @doc "Returns the data about open source dependencies transformed by `f` (or identity if `f` isn't provided)."
  @spec get :: %{}
  @spec get((%{} -> x)) :: x when x: var
  def get(f \\ &(&1)), do: Agent.get(__MODULE__, f)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp read_bom do
    Application.app_dir(:air)
    |> Path.join(bom_path())
    |> File.read!()
    |> Poison.decode!()
  end

  defp bom_path do
    Application.get_env(:air, Air.BOM)
    |> Keyword.fetch!(:location)
  end
end

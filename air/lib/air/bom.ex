defmodule Air.BOM do
  @moduledoc """
  An Agent keeping the data about open source dependencies of the project.  See priv/bom.json.example for how
  the data is structured.
  """

  use Agent, start: {__MODULE__, :start_link, []}


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

  @doc "Returns the path at which the dependencies archive can be found"
  @spec dependencies_path() :: String.t
  def dependencies_path(), do: path_for(:dependencies)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp read_bom do
    path_for(:bom_file)
    |> File.read!()
    |> Poison.decode!()
  end

  defp path_for(type) do
    root_path = Application.app_dir(:air)
    file_name = Application.get_env(:air, Air.BOM) |> Keyword.fetch!(type)
    Path.join([root_path, file_name])
  end
end

defmodule BOM.Gather.Elixir.Hex do
  use GenServer

  def start_link, do: GenServer.start_link(__MODULE__, [], name: __MODULE__)

  def package(name), do: GenServer.call(__MODULE__, {:package, name})

  def handle_call({:package, name}, _from, state) do
    case Hex.API.Package.get(name) do
      {200, result, _headers} -> {:reply, {:ok, result}, state}
      {404, _, _headers} -> {:reply, {:error, :not_found}, state}
    end
  end
end

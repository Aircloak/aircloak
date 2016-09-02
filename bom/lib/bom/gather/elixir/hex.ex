defmodule BOM.Gather.Elixir.Hex do
  use GenServer

  def start_link, do: GenServer.start_link(__MODULE__, [], name: __MODULE__)

  def package(name) do
    send(__MODULE__, {:package, name, self()})

    receive do
      message -> message
    after
      :timer.minutes(1) -> raise "Timeout while fetching from hex.pm"
    end
  end

  def handle_info({:package, name, from}, state) do
    case Hex.API.Package.get(name) do
      {200, result, _headers} -> send(from, {:ok, result})
      {404, _content, _headers} -> send(from, {:error, :not_found})
      {429, _content, _headers} ->
        Process.send_after(self(), {:package, name, from}, :timer.seconds(5))
    end

    {:noreply, state}
  end
end

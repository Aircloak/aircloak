defmodule BOM.Gather.Elixir.Hex do
  use GenServer

  def start_link, do: GenServer.start_link(__MODULE__, [], name: __MODULE__)

  def package(name), do: GenServer.call(__MODULE__, {:package, name}, :timer.minutes(1))

  def handle_call({:package, name}, from, state) do
    case Hex.API.Package.get(name) do
      {200, result, _headers} -> {:reply, {:ok, result}, state}
      {404, _content, _headers} -> {:reply, {:error, :not_found}, state}
      {429, _content, _headers} ->
        retry({:package, name}, from)
        :timer.sleep(:timer.seconds(5))
        {:noreply, state}
    end
  end

  def handle_cast({:retry, {:package, name}, from}, state) do
    case Hex.API.Package.get(name) do
      {200, result, _headers} -> GenServer.reply(from, {:ok, result})
      {404, _content, _headers} -> GenServer.reply(from, {:error, :not_found})
      {429, _content, _headers} ->
        retry({:package, name}, from)
        :timer.sleep(:timer.seconds(5))
    end

    {:noreply, state}
  end

  defp retry(message, from), do: GenServer.cast(__MODULE__, {:retry, message, from})
end

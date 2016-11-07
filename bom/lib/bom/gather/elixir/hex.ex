defmodule BOM.Gather.Elixir.Hex do
  @moduledoc "Wraps Hex, serializing access and taking care of rate limiting."

  use GenServer


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Searches for package metadata on hex.pm. Returns `{:ok, json}` if found, `{:error, :not_found}` otherwise."
  @spec package(String.t) :: {:ok, Map.t} | {:error, :not_found}
  def package(name) do
    GenServer.cast(__MODULE__, {:package, name, self()})

    receive do
      message -> message
    after
      :timer.minutes(2) -> raise "Timeout while fetching from hex.pm"
    end
  end


  # -------------------------------------------------------------------
  # OTP Callbacks
  # -------------------------------------------------------------------

  @doc false
  @spec start_link :: GenServer.on_start
  def start_link, do: GenServer.start_link(__MODULE__, [], name: __MODULE__)

  @doc false
  def handle_cast({:package, name, from}, state) do
    request_package(name, from)
    {:noreply, state}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp request_package(name, from) do
    case Hex.API.Package.get(name) do
      {200, result, _headers} -> send(from, {:ok, result})
      {404, _content, _headers} -> send(from, {:error, :not_found})
      {429, _content, _headers} ->
        :timer.sleep(:timer.seconds(5))
        request_package(name, from)
    end
  end
end

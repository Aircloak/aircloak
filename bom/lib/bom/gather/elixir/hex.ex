defmodule BOM.Gather.Elixir.Hex do
  @moduledoc "Queries hex.pm, serializing access and taking care of rate limiting."

  use GenServer
  @endpoint "https://hex.pm/api/packages"


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
    require Logger

    case do_request_package(name) do
      {200, result} -> send(from, {:ok, result})
      {404, _content} -> send(from, {:error, :not_found})
      {429, _content} -> retry(name, from)
      {:error, %{reason: :timeout}} -> retry(name, from)
      other ->
        Logger.error("Received unexpected response from hex.pm.\n#{inspect(other)}")
        raise "Bad response"
    end
  end

  defp retry(name, from) do
    :timer.sleep(:timer.seconds(5))
    request_package(name, from)
  end

  defp do_request_package(name) do
    with {:ok, response} <- HTTPoison.get("#{@endpoint}/#{name}"),
      {:ok, content} <- Poison.decode(response.body)
    do
      {response.status_code, content}
    end
  end
end

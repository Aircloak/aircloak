defmodule BOM.Gather.Elixir.Hex do
  @moduledoc """
  Retrieving of licenses metadata from repo.hex.pm.

  Note that we're not using hex API, because it does rate limiting. In contrast, we can freely fetch
  from the repo as much as we will, and all the data is available there
  (see [here](https://github.com/hexpm/specifications/blob/master/package_tarball.md#package-tarball)) for
  more info.
  """

  use GenServer
  @endpoint "https://repo.hex.pm/tarballs"


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Returns the licenses for the given package and version."
  @spec licenses(String.t, String.t) :: [String.t] | nil
  def licenses(package_name, version) do
    {:ok, tarball_content} = package_tarball(package_name, version)
    decode_licenses(tarball_content)
  end


  # -------------------------------------------------------------------
  # OTP Callbacks
  # -------------------------------------------------------------------

  @doc false
  @spec start_link :: GenServer.on_start
  def start_link, do: GenServer.start_link(__MODULE__, [], name: __MODULE__)

  @doc false
  def handle_cast({:package, name, version, from}, state) do
    request_package(name, version, from)
    {:noreply, state}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp package_tarball(name, version) do
    GenServer.cast(__MODULE__, {:package, name, version, self()})

    receive do
      message -> message
    after
      :timer.minutes(2) -> raise "Timeout while fetching from hex.pm"
    end
  end

  defp request_package(name, version, from) do
    require Logger

    case do_request_package(name, version) do
      {200, result} -> send(from, {:ok, result})
      {404, _content} -> send(from, {:error, :not_found})
      {429, _content} -> retry(name, version, from)
      {:error, %{reason: :timeout}} -> retry(name, version, from)
      other ->
        Logger.error("Received unexpected response from hex.pm.\n#{inspect(other)}")
        raise "Bad response"
    end
  end

  defp retry(name, version, from) do
    :timer.sleep(:timer.seconds(5))
    request_package(name, version, from)
  end

  defp do_request_package(name, version) do
    with {:ok, response} <- HTTPoison.get("#{@endpoint}/#{name}-#{version}.tar"), do:
      {response.status_code, response.body}
  end

  defp decode_licenses(tarball_content) do
    # Decoding of package tarball. See  https://github.com/hexpm/specifications/blob/master/package_tarball.md
    # for description.

    # decode metadata file
    {:ok, [{'metadata.config', encoded_meta}]} =
      :erl_tar.extract({:binary, tarball_content}, [:memory, files: ['metadata.config']])
    # read terms in the file
    encoded_meta
    |> erl_terms()
    # take licenses k-v pair
    |> Stream.filter(&match?({"licenses", _}, &1))
    |> Stream.map(fn({"licenses", licenses}) -> licenses end)
    |> Enum.take(1)
    |> case do
      [licenses] -> licenses
      [] -> nil
    end
  end

  defp erl_terms(content), do:
    # Given plain content (a list of erlang expressions), returns a lazy stream of evaluated terms.
    # This is similar to `:file.consult/1`, except it works with an in memory string.
    Stream.resource(
      fn ->
        {:ok, pid} = StringIO.open(content)
        {pid, 1}
      end,
      fn({pid, pos}) ->
        case :io.parse_erl_exprs(pid, '', pos) do
          {:eof, _} -> {:halt, {pid, :eof}}
          {:ok, tokenized_exprs, end_location} ->
            {:value, value, _} = :erl_eval.exprs(tokenized_exprs, [])
            {[value], {pid, end_location}}
        end
      end,
      fn {pid, _} -> StringIO.close(pid) end
    )
end

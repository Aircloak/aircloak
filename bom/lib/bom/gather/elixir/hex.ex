defmodule BOM.Gather.Elixir.Hex do
  @moduledoc """
  Retrieving of licenses metadata from repo.hex.pm.

  Note that we're not using hex API, because it does rate limiting. In contrast, we can freely fetch
  from the repo as much as we will, and all the data is available there
  (see [here](https://github.com/hexpm/specifications/blob/master/package_tarball.md#package-tarball)) for
  more info.
  """

  @endpoint "https://repo.hex.pm/tarballs"

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Returns the licenses for the given package and version."
  @spec licenses(String.t(), String.t()) :: [String.t()] | nil
  def licenses(package_name, version) do
    {:ok, tarball_content} = package_tarball(package_name, version)
    decode_licenses(tarball_content)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp package_tarball(name, version) do
    require Logger

    case request_package(name, version) do
      {200, result} ->
        {:ok, result}

      {404, _content} ->
        {:error, :not_found}

      {:error, %{reason: :timeout}} ->
        :timer.sleep(:timer.seconds(5))
        package_tarball(name, version)

      other ->
        Logger.error("Received unexpected response.\n#{inspect(other)}")
        raise "Bad response"
    end
  end

  defp request_package(name, version) do
    options =
      case System.get_env("HTTPS_PROXY") do
        nil -> []
        proxy -> [proxy: proxy]
      end

    with {:ok, response} <- HTTPoison.get("#{@endpoint}/#{name}-#{version}.tar", [], options),
         do: {response.status_code, response.body}
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
    |> Stream.map(fn {"licenses", licenses} -> licenses end)
    |> Enum.take(1)
    |> case do
      [licenses] -> licenses
      [] -> nil
    end
  end

  defp erl_terms(content),
    # Given plain content (a list of erlang expressions), returns a lazy stream of evaluated terms.
    # This is similar to `:file.consult/1`, except it works with an in memory string.
    do:
      Stream.resource(
        fn ->
          {:ok, pid} = StringIO.open(content)
          {pid, 1}
        end,
        fn {pid, pos} ->
          case :io.parse_erl_exprs(pid, '', pos) do
            {:eof, _} ->
              {:halt, {pid, :eof}}

            {:ok, tokenized_exprs, end_location} ->
              {:value, value, _} = :erl_eval.exprs(tokenized_exprs, [])
              {[value], {pid, end_location}}
          end
        end,
        fn {pid, _} -> StringIO.close(pid) end
      )
end

defmodule BOM.Util do
  @moduledoc "Small utilities required for BOM."

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Creates a temporary directory with a subdirectory with the given name. Returns the path to the subdirectory."
  @spec create_temp_dir(String.t()) :: String.t()
  def create_temp_dir(name) do
    tmp_dir_path = Path.join([System.tmp_dir!(), name])
    File.rm_rf!(tmp_dir_path)
    File.mkdir!(tmp_dir_path)
    tmp_dir_path
  end

  @doc "Gets the given https url, using proxy if the HTTPS_PROXY system var is set."
  @spec https_get(String.t(), Keyword.t()) :: {integer, String.t()} | {:error, any}
  def https_get(url, options \\ []) do
    with {:ok, {{_, status_code, _}, _headers, body}} <- :httpc.request(to_charlist(url), httpc_profile()),
         do: {status_code, :erlang.list_to_binary(body)}
  end

  # -------------------------------------------------------------------
  # httpc helpers
  # -------------------------------------------------------------------

  defp httpc_profile() do
    case :inets.start(:httpc, profile: __MODULE__) do
      {:ok, pid} ->
        :httpc.set_options(httpc_options(), pid)
        pid

      {:error, {:already_started, pid}} ->
        pid
    end
  end

  defp httpc_options() do
    case proxy() do
      {:ok, proxy} -> [proxy: {proxy, []}, https_proxy: {proxy, []}]
      _ -> []
    end
  end

  defp proxy() do
    case System.get_env("HTTPS_PROXY") do
      "http://" <> proxy ->
        [host, port] = String.split(proxy, ":")
        {:ok, {to_charlist(host), String.to_integer(port)}}

      _ ->
        :error
    end
  end
end

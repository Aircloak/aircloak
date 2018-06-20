defmodule BOM.Util do
  @moduledoc "Small utilities required for BOM."

  @doc "Creates a temporary directory with a subdirectory with the given name. Returns the path to the subdirectory."
  @spec create_temp_dir(String.t()) :: String.t()
  def create_temp_dir(name) do
    tmp_dir_path = Path.join([System.tmp_dir!(), name])
    File.rm_rf!(tmp_dir_path)
    File.mkdir!(tmp_dir_path)
    tmp_dir_path
  end

  @doc "Gets the given https url, using proxy if the HTTPS_PROXY system var is set."
  @spec https_get(String.t(), Keyword.t()) :: {integer, String.t()}
  def https_get(url, options \\ []) do
    options =
      case System.get_env("HTTPS_PROXY") do
        nil -> options
        proxy -> [{:proxy, proxy} | options]
      end

    with {:ok, response} <- HTTPoison.get(url, _headers = [], options),
         do: {response.status_code, response.body}
  end
end

defmodule AirWeb.Plugs.Utils do
  @moduledoc """
  Utilities for plug writing
  """

  @doc """
  If the request is for a resource at or more deeply within the provided path,
  the provided callback function is run and its return value returned.
  Otherwise the connection is returned unaltered.

  Example:

    if_in_section(conn, "/admin/privacy_policy", callback)

  For a request to `/data_sources` the conn is returned unaltered, whereas
  requests for `/admin/privacy_policy` and `/admin/privacy_policy/new` both invoke
  the callback
  """
  @spec if_in_section(Plug.Conn.t(), String.t(), (() -> Plug.Conn.t())) :: Plug.Conn.t()
  def if_in_section(conn, path_prefix, callback) do
    path_regex = Regex.compile!("^#{path_prefix}")

    if Regex.match?(path_regex, conn.request_path) do
      conn
    else
      callback.()
    end
  end
end

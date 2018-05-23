defmodule Central.Plug.KibanaProxyParser do
  @moduledoc """
  Post and Put payload bodies can only be read once.
  This parser caches the un-modified body for requests
  that will be proxied, so they can be forwarded in
  their original form.
  """

  @behaviour Plug.Parsers
  import Plug.Conn

  @impl Plug.Parsers
  def init(opts), do: opts

  @impl Plug.Parsers
  def parse(conn, _type, _subtype, _headers, options) do
    case conn.path_info do
      ["kibana" | _] ->
        conn
        |> read_body(options)
        |> case do
          {:ok, body, conn} -> {:ok, %{}, put_private(conn, :raw_body, body)}
          {:more, _, conn} -> {:error, :too_large, conn}
        end

      _ ->
        {:next, conn}
    end
  end
end

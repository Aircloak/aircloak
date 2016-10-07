defmodule Air.PsqlServer.Protocol.Messages do
  @moduledoc false

  # Helpers for working with PostgreSQL protocol messages described in
  # https://www.postgresql.org/docs/9.6/static/protocol-message-formats.html.
  # These functions basically belong to Air.PsqlServer.Protocol, but they're extracted into
  # a separate module so we can reuse them in tests.

  def authentication_method(:cleartext), do: <<?R, 8::32, 3::32>>

  def authentication_ok(), do: <<?R, 8::32, 0::32>>

  def fatal_error(code, message), do:
    message_with_size(?E, <<
      ?S, "FATAL", 0,
      ?C, code::binary, 0,
      ?M, message::binary, 0,
      0
    >>)

  def require_ssl(), do: <<?S>>

  def ready_for_query(), do: <<?Z, 5::32, ?I>>

  def parameter_status(name, value), do:
    message_with_size(?S, <<name::binary, 0, value::binary, 0>>)

  def password_length(<<?p, length::32>>), do: length - 4

  def ssl_message?(<<8::32, 1234::16, 5679::16>>), do: true
  def ssl_message?(_), do: false

  def startup_message(<<length::32, major::16, minor::16>> = message), do:
    %{length: length - byte_size(message), version: %{major: major, minor: minor}}

  def null_terminated_to_string(null_terminated_string) do
    string_size = byte_size(null_terminated_string) - 1
    <<string::binary-size(string_size), 0>> = null_terminated_string
    string
  end

  def login_params(raw_login_params) do
    raw_login_params
    |> String.split(<<0>>)
    |> Stream.chunk(2)
    |> Stream.map(&List.to_tuple/1)
    |> Enum.into(%{})
  end

  defp message_with_size(type, payload), do:
    <<type, (4 + byte_size(payload))::32, payload::binary>>
end

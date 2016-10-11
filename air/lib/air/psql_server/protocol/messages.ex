defmodule Air.PsqlServer.Protocol.Messages do
  @moduledoc false

  # Helpers for working with PostgreSQL protocol messages described in
  # https://www.postgresql.org/docs/9.6/static/protocol-message-formats.html.
  # These functions basically belong to Air.PsqlServer.Protocol, but they're extracted into
  # a separate module so we can reuse them in tests.


  #-----------------------------------------------------------------------------------------------------------
  # Decoding functions
  #-----------------------------------------------------------------------------------------------------------

  def ssl_message?(message), do: message == ssl_message()

  def parse_startup_message(<<length::32, major::16, minor::16>> = message), do:
    %{length: length - byte_size(message), version: %{major: major, minor: minor}}

  def parse_message_header(<<message_byte::8, length::32>>), do:
    %{type: message_name(message_byte), length: length - 4}

  def parse_login_params(raw_login_params) do
    raw_login_params
    |> String.split(<<0>>)
    |> Stream.chunk(2)
    |> Stream.map(&List.to_tuple/1)
    |> Enum.into(%{})
  end


  #-----------------------------------------------------------------------------------------------------------
  # Encoding functions
  #-----------------------------------------------------------------------------------------------------------

  def authentication_method(:cleartext), do: message_with_size(:authentication, <<3::32>>)

  def authentication_ok(), do: message_with_size(:authentication, <<0::32>>)

  def command_complete(tag), do: message_with_size(:command_complete, null_terminate(tag))

  def fatal_error(code, message), do:
    message_with_size(:error_response, <<
      ?S, null_terminate("FATAL")::binary,
      ?C, null_terminate(code)::binary,
      ?M, null_terminate(message)::binary,
      0
    >>)

  def require_ssl(), do: <<?S>>

  def ready_for_query(), do: message_with_size(:ready_for_query, <<?I>>)

  def parameter_status(name, value), do:
    message_with_size(:parameter_status, null_terminate(name) <> null_terminate(value))

  def password_message(password), do: message_with_size(:password, null_terminate(password))

  def query_message(query), do: message_with_size(:query, null_terminate(query))

  def ssl_message(), do: message_with_size(<<1234::16, 5679::16>>)

  def startup_message(major, minor, opts) do
    [
      <<major::16, minor::16>>,
      Enum.map(opts, fn({key, value}) -> null_terminate(to_string(key)) <> null_terminate(value) end)
    ]
    |> to_string()
    |> message_with_size()
  end

  def terminate_message(), do: message_with_size(:terminate, <<>>)


  #-----------------------------------------------------------------------------------------------------------
  # Message helpers
  #-----------------------------------------------------------------------------------------------------------

  defp message_with_size(payload), do:
    <<(4 + byte_size(payload))::32, payload::binary>>

  defp message_with_size(message_name, payload), do:
    <<message_byte(message_name)::8, message_with_size(payload)::binary>>

  for {message_name, message_byte} <-
      %{
        authentication: ?R,
        command_complete: ?C,
        error_response: ?E,
        password: ?p,
        parameter_status: ?S,
        query: ?Q,
        ready_for_query: ?Z,
        terminate: ?X
      } do
    defp message_name(unquote(message_byte)), do: unquote(message_name)
    defp message_byte(unquote(message_name)), do: unquote(message_byte)
  end


  #-----------------------------------------------------------------------------------------------------------
  # Other helpers
  #-----------------------------------------------------------------------------------------------------------

  def null_terminated_to_string(null_terminated_string) do
    string_size = byte_size(null_terminated_string) - 1
    <<string::binary-size(string_size), 0>> = null_terminated_string
    string
  end

  defp null_terminate(string), do: <<string::binary, 0>>
end

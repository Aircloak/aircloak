defmodule Air.PsqlServer.Protocol.Messages do
  @moduledoc false

  # Helpers for working with PostgreSQL protocol messages described in
  # https://www.postgresql.org/docs/9.6/static/protocol-message-formats.html.
  # These functions basically belong to Air.PsqlServer.Protocol, but they're extracted into
  # a separate module so we can reuse them in tests.


  #-----------------------------------------------------------------------------------------------------------
  # Incoming (frontend) messages
  #-----------------------------------------------------------------------------------------------------------

  def ssl_message?(message), do: message == ssl_message()

  def decode_startup_message(<<length::32, major::16, minor::16>> = message), do:
    %{length: length - byte_size(message), version: %{major: major, minor: minor}}

  def decode_message_header(<<message_byte::8, length::32>>), do:
    %{type: frontend_message_name(message_byte), length: length - 4}

  def decode_login_params(raw_login_params) do
    raw_login_params
    |> String.split(<<0>>)
    |> Stream.chunk(2)
    |> Stream.map(&List.to_tuple/1)
    |> Enum.into(%{})
  end

  def query_message(query), do: frontend_message(:query, null_terminate(query))

  def password_message(password), do: frontend_message(:password, null_terminate(password))

  def terminate_message(), do: frontend_message(:terminate, <<>>)

  for {message_name, message_byte} <-
      %{
        password: ?p,
        query: ?Q,
        terminate: ?X
      } do
    defp frontend_message_name(unquote(message_byte)), do: unquote(message_name)
    defp frontend_message_byte(unquote(message_name)), do: unquote(message_byte)
  end

  defp frontend_message(message_name, payload), do:
    <<frontend_message_byte(message_name)::8, message_with_size(payload)::binary>>


  #-----------------------------------------------------------------------------------------------------------
  # Outgoing (backend messages)
  #-----------------------------------------------------------------------------------------------------------

  def backend_message_type(<<message_byte::8, _::binary>>), do:
    backend_message_name(message_byte)

  def authentication_method(:cleartext), do: backend_message(:authentication, <<3::32>>)

  def authentication_ok(), do: backend_message(:authentication, <<0::32>>)

  def command_complete(tag), do: backend_message(:command_complete, null_terminate(tag))

  def data_row(values) do
    encoded_row =
      values
      |> Enum.map(&value_to_text/1)
      |> IO.iodata_to_binary()

    backend_message(:data_row, <<length(values)::16, encoded_row::binary>>)
  end

  def error_message(severity, code, message), do:
    backend_message(:error_response, <<
      ?S, null_terminate(severity)::binary,
      ?C, null_terminate(code)::binary,
      ?M, null_terminate(message)::binary,
      0
    >>)

  def require_ssl(), do: <<?S>>

  def ready_for_query(), do: backend_message(:ready_for_query, <<?I>>)

  def row_description(columns) do
    columns_descriptions =
      columns
      |> Enum.map(&column_description/1)
      |> IO.iodata_to_binary()

    backend_message(:row_description, <<length(columns)::16, columns_descriptions::binary>>)
  end

  def parameter_status(name, value), do:
    backend_message(:parameter_status, null_terminate(name) <> null_terminate(value))

  def ssl_message(), do: message_with_size(<<1234::16, 5679::16>>)

  def startup_message(major, minor, opts) do
    [
      <<major::16, minor::16>>,
      Enum.map(opts, fn({key, value}) -> null_terminate(to_string(key)) <> null_terminate(value) end)
    ]
    |> to_string()
    |> message_with_size()
  end

  for {message_name, message_byte} <-
      %{
        authentication: ?R,
        command_complete: ?C,
        data_row: ?D,
        error_response: ?E,
        parameter_status: ?S,
        ready_for_query: ?Z,
        row_description: ?T,
      } do
    defp backend_message_name(unquote(message_byte)), do: unquote(message_name)
    defp backend_message_byte(unquote(message_name)), do: unquote(message_byte)
  end

  defp backend_message(message_name, payload), do:
    <<backend_message_byte(message_name)::8, message_with_size(payload)::binary>>


  #-----------------------------------------------------------------------------------------------------------
  # Types encoding
  #-----------------------------------------------------------------------------------------------------------

  for {type, meta} <- %{
    # Obtained as `select typname, oid, typlen from pg_type`
    int8: %{oid: 20, len: 8},
    int4: %{oid: 23, len: 4},
    text: %{oid: 25, len: -1},
    unknown: %{oid: 705, len: -1}
  } do
    defp column_description(%{type: unquote(type)} = column), do:
      <<
        null_terminate(column.name)::binary,
        0::32,
        0::16,
        unquote(meta.oid)::32,
        unquote(meta.len)::16,
        -1::32,
        0::16
      >>
  end

  defp value_to_text(nil), do: <<-1::32>>
  defp value_to_text(other) do
    string_representation = to_string(other)
    <<byte_size(string_representation)::32, string_representation::binary>>
  end


  #-----------------------------------------------------------------------------------------------------------
  # Other helpers
  #-----------------------------------------------------------------------------------------------------------

  defp message_with_size(payload), do:
    <<(4 + byte_size(payload))::32, payload::binary>>

  def null_terminated_to_string(null_terminated_string) do
    string_size = byte_size(null_terminated_string) - 1
    <<string::binary-size(string_size), 0>> = null_terminated_string
    string
  end

  defp null_terminate(string), do: <<string::binary, 0>>
end

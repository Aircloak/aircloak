defmodule Air.PsqlServer.Protocol.Messages do
  @moduledoc """
  Encoding and decoding of PostgreSQL protocol messages.

  Formats are described [here](https://www.postgresql.org/docs/9.6/static/protocol-message-formats.html).
  """

  alias Air.PsqlServer.{Protocol, ConnectionRegistry}

  @type server_message ::
          {:authentication_method, :cleartext}
          | :authentication_ok
          | :bind_complete
          | :close_complete
          | {:command_complete, String.t()}
          | {:syntax_error, String.t()}
          | {:fatal_error, String.t()}
          | :query_cancelled
          | :ready_for_query
          | :parse_complete
          | :require_ssl
          | :ssl_not_supported
          | {:notice, String.t()}
          | {:backend_key_data, non_neg_integer, non_neg_integer}
          | {:parameter_status, String.t(), String.t()}
          | {:parameter_description, [Protocol.Value.type()]}
          | {:row_description, [Protocol.column()], [Protocol.Value.format()]}
          | {:data_row, [Protocol.db_value()], [Protocol.Value.type()], [Protocol.Value.format()]}
          | :no_data

  @type client_message_name ::
          :bind
          | :close
          | :describe
          | :execute
          | :flush
          | :parse
          | :password
          | :query
          | :sync
          | :terminate
  @type client_message_header :: %{type: client_message_name, length: non_neg_integer}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Determines if the client message is an ssl_message."
  @spec ssl_message?(binary) :: boolean
  def ssl_message?(message), do: message == message_with_size(<<1234::16, 5679::16>>)

  @doc "Determines if the client message is a cancel request"
  @spec cancel_message?(binary) :: boolean
  def cancel_message?(<<16::32, 1234::16, 5678::16, _payload::binary>>), do: true
  def cancel_message?(_message), do: false

  @doc "Decodes the cancel message."
  @spec decode_cancel_message_parameters(binary) :: ConnectionRegistry.key_data()
  def decode_cancel_message_parameters(<<process_id::32, secret_key::32>>),
    do: %{process_id: process_id, secret_key: secret_key}

  @doc "Decodes the startup message."
  @spec decode_startup_message(<<_::64>>) :: %{
          length: non_neg_integer,
          version: %{major: non_neg_integer, minor: non_neg_integer}
        }
  def decode_startup_message(<<length::32, major::16, minor::16>> = message),
    do: %{length: length - byte_size(message), version: %{major: major, minor: minor}}

  @doc "Decodes the message header of the client message."
  @spec decode_message_header(<<_::40>>) :: client_message_header
  def decode_message_header(<<message_byte::8, length::32>>),
    do: %{type: client_message_name(message_byte), length: length - 4}

  @doc "Decodes the login_params message."
  @spec decode_login_params(binary) :: %{String.t() => String.t()}
  def decode_login_params(raw_login_params) do
    raw_login_params
    |> String.split(<<0>>)
    |> Stream.chunk(2)
    |> Stream.map(&List.to_tuple/1)
    |> Enum.into(%{})
  end

  @doc "Decodes the payload of the client message with the given name."
  @spec decode_message(client_message_name, binary) :: map
  def decode_message(:close, <<type, name::binary>>),
    do: %{type: decode_statement_or_portal(type), name: hd(:binary.split(name, <<0>>))}

  def decode_message(:bind, message), do: decode_bind_message(message)

  def decode_message(:describe, <<type, describe_data::binary>>) do
    [name, ""] = :binary.split(describe_data, <<0>>)
    %{type: decode_statement_or_portal(type), name: name}
  end

  def decode_message(:execute, execute_data) do
    [portal, <<max_rows::32>>] = :binary.split(execute_data, <<0>>)
    %{portal: portal, max_rows: max_rows}
  end

  def decode_message(:parse, parse_data) do
    [name, parse_data] = :binary.split(parse_data, <<0>>)
    [query, parse_data] = :binary.split(parse_data, <<0>>)
    <<num_params::16, parse_data::binary>> = parse_data

    param_types =
      for <<oid::32 <- parse_data>> do
        if oid == 0, do: :unknown, else: Protocol.Value.type_from_oid(oid)
      end

    %{
      name: name,
      query: query,
      num_params: num_params,
      param_types: param_types
    }
  end

  def decode_message(:password, password_message) do
    [password, <<>>] = :binary.split(password_message, <<0>>)
    password
  end

  def decode_message(:query, query_message) do
    [query, <<>>] = :binary.split(query_message, <<0>>)
    query
  end

  @doc "Encodes the server message."
  @spec encode_message(server_message) :: binary
  def encode_message({:authentication_method, :cleartext}), do: server_message(:authentication, <<3::32>>)

  def encode_message(:authentication_ok), do: server_message(:authentication, <<0::32>>)

  def encode_message({:backend_key_data, process_id, secret_key}),
    do: server_message(:backend_key_data, <<process_id::32, secret_key::32>>)

  def encode_message(:bind_complete), do: server_message(:bind_complete, <<>>)
  def encode_message(:close_complete), do: server_message(:close_complete, <<>>)

  def encode_message({:command_complete, tag}), do: server_message(:command_complete, null_terminate(tag))

  def encode_message({:data_row, row, column_types, formats}),
    do:
      server_message(
        :data_row,
        <<length(row)::16, encode_row(row, formats, column_types)::binary>>
      )

  def encode_message(:query_cancelled), do: notice_message("NOTICE", "57014", "The query was cancelled.")

  def encode_message({:notice, notice_content}), do: notice_message("NOTICE", "57014", notice_content)

  def encode_message({:syntax_error, error}), do: error_message("ERROR", "42601", error)
  def encode_message({:fatal_error, reason}), do: error_message("FATAL", "28000", reason)
  def encode_message(:ready_for_query), do: server_message(:ready_for_query, <<?I>>)
  def encode_message(:parse_complete), do: server_message(:parse_complete, <<>>)
  def encode_message(:require_ssl), do: <<?S>>
  def encode_message(:ssl_not_supported), do: <<?N>>

  def encode_message({:row_description, columns, result_codes}) do
    columns_descriptions =
      columns
      |> Enum.zip(Stream.cycle(result_codes))
      |> Enum.map(&column_description/1)
      |> IO.iodata_to_binary()

    server_message(:row_description, <<length(columns)::16, columns_descriptions::binary>>)
  end

  def encode_message({:parameter_status, name, value}),
    do: server_message(:parameter_status, null_terminate(name) <> null_terminate(value))

  def encode_message({:parameter_description, param_types}) do
    encoded_types =
      param_types
      |> Enum.map(&Protocol.Value.type_info(&1).oid)
      |> Enum.map(&<<&1::32>>)
      |> IO.iodata_to_binary()

    server_message(:parameter_description, <<length(param_types)::16, encoded_types::binary>>)
  end

  def encode_message(:no_data), do: server_message(:no_data)

  @doc "Converts query parameters to proper Elixir types."
  @spec convert_params([any], [Protocol.Value.format()], [Protocol.Value.type()]) :: [any]
  def convert_params(params, format_codes, param_types) do
    # per protocol, if param types are empty, all parameters are encoded as text
    param_types = if param_types == [], do: Enum.map(params, fn _ -> :text end), else: param_types
    true = length(params) == length(param_types)

    Enum.map(Enum.zip([params, format_codes, param_types]), fn {value, format, type} ->
      Protocol.Value.decode(value, format, type)
    end)
  end

  # -------------------------------------------------------------------
  # Client (aka frontend) message headers
  # -------------------------------------------------------------------

  for {message_name, message_byte} <-
        %{
          bind: ?B,
          close: ?C,
          describe: ?D,
          execute: ?E,
          flush: ?H,
          parse: ?P,
          password: ?p,
          query: ?Q,
          sync: ?S,
          terminate: ?X
        } do
    defp client_message_name(unquote(message_byte)), do: unquote(message_name)
    defp client_message_byte(unquote(message_name)), do: unquote(message_byte)
  end

  # -------------------------------------------------------------------
  # Decoding of a bind message
  # -------------------------------------------------------------------

  defp decode_bind_message(bind_message_data) do
    [portal, bind_message_data] = :binary.split(bind_message_data, <<0>>)
    [name, bind_message_data] = :binary.split(bind_message_data, <<0>>)

    <<num_format_codes::16, bind_message_data::binary>> = bind_message_data
    {format_codes, bind_message_data} = decode_format_codes(num_format_codes, bind_message_data)

    <<num_params::16, bind_message_data::binary>> = bind_message_data
    {params, bind_message_data} = decode_params(num_params, bind_message_data)

    <<num_result_codes::16, bind_message_data::binary>> = bind_message_data
    {result_codes, <<>>} = decode_result_codes(num_result_codes, bind_message_data)

    %{
      portal: portal,
      name: name,
      format_codes: format_codes,
      params: params,
      result_codes: result_codes
    }
  end

  defp decode_format_codes(0, bind_message_data), do: {[], bind_message_data}

  defp decode_format_codes(num_format_codes, <<format_code::16, bind_message_data::binary>>) do
    {rest_codes, bind_message_data} = decode_format_codes(num_format_codes - 1, bind_message_data)
    {[decode_format_code(format_code) | rest_codes], bind_message_data}
  end

  defp decode_format_code(0), do: :text
  defp decode_format_code(1), do: :binary

  defp decode_result_codes(0, bind_message_data), do: {[], bind_message_data}

  defp decode_result_codes(num_result_codes, <<result_code::16, bind_message_data::binary>>) do
    {rest_codes, bind_message_data} = decode_result_codes(num_result_codes - 1, bind_message_data)
    {[decode_format_code(result_code) | rest_codes], bind_message_data}
  end

  defp decode_params(0, bind_message_data), do: {[], bind_message_data}

  defp decode_params(num_params, bind_message_data) do
    {value, bind_message_data} = param_value(bind_message_data)
    {rest_values, bind_message_data} = decode_params(num_params - 1, bind_message_data)
    {[value | rest_values], bind_message_data}
  end

  defp param_value(<<-1::signed-integer-size(32), bind_message_data::binary>>), do: {nil, bind_message_data}

  defp param_value(<<size::32, value::binary-size(size), bind_message_data::binary>>), do: {value, bind_message_data}

  # -------------------------------------------------------------------
  # Server (i.e. "backend" as referenced in official PostgreSQL docs) messages
  # -------------------------------------------------------------------

  for {message_name, message_byte} <-
        %{
          authentication: ?R,
          backend_key_data: ?K,
          bind_complete: ?2,
          close_complete: ?3,
          command_complete: ?C,
          data_row: ?D,
          no_data: ?n,
          error_response: ?E,
          notice_response: ?N,
          parameter_description: ?t,
          parameter_status: ?S,
          parse_complete: ?1,
          ready_for_query: ?Z,
          row_description: ?T
        } do
    defp server_message_byte(unquote(message_name)), do: unquote(message_byte)
  end

  defp server_message(message_name, payload \\ <<>>),
    do: <<server_message_byte(message_name)::8, message_with_size(payload)::binary>>

  defp notice_message(severity, code, message), do: response_with_message(:notice_response, severity, code, message)

  defp error_message(severity, code, message), do: response_with_message(:error_response, severity, code, message)

  defp response_with_message(type, severity, code, message),
    do:
      server_message(type, <<
        ?S,
        null_terminate(severity)::binary,
        ?C,
        null_terminate(code)::binary,
        ?M,
        null_terminate(message)::binary,
        0
      >>)

  # -------------------------------------------------------------------
  # Row encoding
  # -------------------------------------------------------------------

  defp column_description({column, result_code}),
    do: <<
      null_terminate(column.name)::binary,
      0::32,
      0::16,
      Protocol.Value.type_info(column.type).oid::32,
      Protocol.Value.type_info(column.type).len::16,
      -1::32,
      encode_format_code(result_code)::16
    >>

  defp encode_format_code(:text), do: 0
  defp encode_format_code(:binary), do: 1

  defp encode_row(row, formats, column_types),
    do:
      Enum.zip([row, Stream.cycle(formats), column_types])
      |> Enum.map(fn {value, format, type} -> Protocol.Value.encode(value, format, type) end)
      |> IO.iodata_to_binary()

  # -------------------------------------------------------------------
  # Other helpers
  # -------------------------------------------------------------------

  defp message_with_size(payload), do: <<4 + byte_size(payload)::32, payload::binary>>

  defp null_terminate(string), do: <<string::binary, 0>>

  defp decode_statement_or_portal(?S), do: :statement
  defp decode_statement_or_portal(?P), do: :portal
end

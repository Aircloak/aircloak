defmodule Air.PsqlServer.Protocol.Messages do
  @moduledoc false

  # Helpers for working with PostgreSQL protocol messages described in
  # https://www.postgresql.org/docs/9.6/static/protocol-message-formats.html.
  # These functions basically belong to Air.PsqlServer.Protocol, but they're extracted into
  # a separate module so we can reuse them in tests.


  #-----------------------------------------------------------------------------------------------------------
  # Client messages (i.e. "frontend" as referenced in official PostgreSQL docs)
  #-----------------------------------------------------------------------------------------------------------

  def ssl_message?(message), do: message == ssl_message()

  def decode_startup_message(<<length::32, major::16, minor::16>> = message), do:
    %{length: length - byte_size(message), version: %{major: major, minor: minor}}

  def decode_message_header(<<message_byte::8, length::32>>), do:
    %{type: client_message_name(message_byte), length: length - 4}

  def decode_login_params(raw_login_params) do
    raw_login_params
    |> String.split(<<0>>)
    |> Stream.chunk(2)
    |> Stream.map(&List.to_tuple/1)
    |> Enum.into(%{})
  end

  def decode_message(:close, <<type, name::binary>>), do:
    %{type: type, name: name}
  def decode_message(:bind, message), do:
    decode_bind_message(message)
  def decode_message(:describe, <<type, describe_data::binary>>) do
    [name, ""] = :binary.split(describe_data, <<0>>)
    %{type: type, name: name}
  end
  def decode_message(:execute, execute_data) do
    [name, <<max_rows::32>>] = :binary.split(execute_data, <<0>>)
    %{name: name, max_rows: max_rows}
  end
  def decode_message(:parse, parse_data) do
    [name, parse_data] = :binary.split(parse_data, <<0>>)
    [query, parse_data] = :binary.split(parse_data, <<0>>)
    <<num_params::16, parse_data::binary>> = parse_data
    param_types = for <<oid::32 <- parse_data>>, do: type_name(oid)

    %{
      name: name,
      query: query,
      num_params: num_params,
      param_types: param_types,
      params: nil,
      parsed_param_types: [],
      result_codes: nil
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

  # we're not using all patterns in this function, so dialyzer complains
  @dialyzer {:nowarn_function, client_message_byte: 1}


  #-----------------------------------------------------------------------------------------------------------
  # Decoding of a bind message
  #-----------------------------------------------------------------------------------------------------------

  defp decode_bind_message(bind_message_data) do
    [_portal, bind_message_data] = :binary.split(bind_message_data, <<0>>)
    [name, bind_message_data] = :binary.split(bind_message_data, <<0>>)

    <<num_format_codes::16, bind_message_data::binary>> = bind_message_data
    {format_codes, bind_message_data} = decode_format_codes(num_format_codes, bind_message_data)

    <<num_params::16, bind_message_data::binary>> = bind_message_data
    {params, bind_message_data} = decode_params(num_params, bind_message_data)

    <<num_result_codes::16, bind_message_data::binary>> = bind_message_data
    {result_codes, <<>>} = decode_result_codes(num_result_codes, bind_message_data)

    %{
      name: name,
      format_codes: format_codes,
      params: params,
      result_codes: result_codes
    }
  end

  defp decode_format_codes(0, bind_message_data), do:
    {[], bind_message_data}
  defp decode_format_codes(num_format_codes, <<format_code::16, bind_message_data::binary>>) do
    {rest_codes, bind_message_data} = decode_format_codes(num_format_codes - 1, bind_message_data)
    {[decode_format_code(format_code) | rest_codes], bind_message_data}
  end

  defp decode_format_code(0), do: :text
  defp decode_format_code(1), do: :binary

  defp decode_result_codes(0, bind_message_data), do:
    {[], bind_message_data}
  defp decode_result_codes(num_result_codes, <<result_code::16, bind_message_data::binary>>) do
    {rest_codes, bind_message_data} = decode_result_codes(num_result_codes - 1, bind_message_data)
    {[decode_format_code(result_code) | rest_codes], bind_message_data}
  end

  defp decode_params(0, bind_message_data), do:
    {[], bind_message_data}
  defp decode_params(num_params, bind_message_data) do
    {value, bind_message_data} = param_value(bind_message_data)
    {rest_values, bind_message_data} = decode_params(num_params - 1, bind_message_data)
    {[value | rest_values], bind_message_data}
  end

  defp param_value(<<-1::signed-integer-size(32), bind_message_data::binary>>), do:
    {nil, bind_message_data}
  defp param_value(<<size::32, value::binary-size(size), bind_message_data::binary>>), do:
    {value, bind_message_data}


  #-----------------------------------------------------------------------------------------------------------
  # Server (i.e. "backend" as referenced in official PostgreSQL docs)
  #-----------------------------------------------------------------------------------------------------------

  def server_message_type(<<message_byte::8, _::binary>>), do:
    server_message_name(message_byte)

  def authentication_method(:cleartext), do: server_message(:authentication, <<3::32>>)

  def authentication_ok(), do: server_message(:authentication, <<0::32>>)

  def bind_complete(), do: server_message(:bind_complete, <<>>)

  def close_complete(), do: server_message(:close_complete, <<>>)

  def command_complete(tag), do: server_message(:command_complete, null_terminate(tag))

  def data_row(values) do
    encoded_row =
      values
      |> Enum.map(&<<byte_size(&1)::32, &1::binary>>)
      |> IO.iodata_to_binary()

    server_message(:data_row, <<length(values)::16, encoded_row::binary>>)
  end

  def error_message(severity, code, message), do:
    server_message(:error_response, <<
      ?S, null_terminate(severity)::binary,
      ?C, null_terminate(code)::binary,
      ?M, null_terminate(message)::binary,
      0
    >>)

  def ready_for_query(), do: server_message(:ready_for_query, <<?I>>)

  def parse_complete(), do: server_message(:parse_complete, <<>>)

  def require_ssl(), do: <<?S>>

  def row_description(columns, result_codes) do
    columns_descriptions =
      columns
      |> Enum.zip(Stream.cycle(result_codes))
      |> Enum.map(&column_description/1)
      |> IO.iodata_to_binary()

    server_message(:row_description, <<length(columns)::16, columns_descriptions::binary>>)
  end

  def parameter_status(name, value), do:
    server_message(:parameter_status, null_terminate(name) <> null_terminate(value))

  def parameter_description(param_types) do
    encoded_types =
      param_types
      |> Enum.map(&type_oid/1)
      |> Enum.map(&<<&1::32>>)
      |> IO.iodata_to_binary()

    server_message(:parameter_description, <<length(param_types)::16, encoded_types::binary>>)
  end

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
        bind_complete: ?2,
        close_complete: ?3,
        command_complete: ?C,
        data_row: ?D,
        error_response: ?E,
        parameter_description: ?t,
        parameter_status: ?S,
        parse_complete: ?1,
        ready_for_query: ?Z,
        row_description: ?T,
      } do
    defp server_message_name(unquote(message_byte)), do: unquote(message_name)
    defp server_message_byte(unquote(message_name)), do: unquote(message_byte)
  end

  defp server_message(message_name, payload), do:
    <<server_message_byte(message_name)::8, message_with_size(payload)::binary>>


  #-----------------------------------------------------------------------------------------------------------
  # Types encoding
  #-----------------------------------------------------------------------------------------------------------

  for {type, meta} <- %{
    # Obtained as `select typname, oid, typlen from pg_type`
    boolean: %{oid: 16, len: 1},
    int8: %{oid: 20, len: 8},
    int4: %{oid: 23, len: 4},
    text: %{oid: 25, len: -1},
    unknown: %{oid: 705, len: -1}
  } do
    defp column_description({%{type: unquote(type)} = column, result_code}), do:
      <<
        null_terminate(column.name)::binary,
        0::32,
        0::16,
        unquote(meta.oid)::32,
        unquote(meta.len)::16,
        -1::32,
        encode_format_code(result_code)::16
      >>

    defp type_name(unquote(meta.oid)), do: unquote(type)
    defp type_oid(unquote(type)), do: unquote(meta.oid)
  end

  defp encode_format_code(:text), do: 0
  defp encode_format_code(:binary), do: 1


  #-----------------------------------------------------------------------------------------------------------
  # Other helpers
  #-----------------------------------------------------------------------------------------------------------

  defp message_with_size(payload), do:
    <<(4 + byte_size(payload))::32, payload::binary>>

  defp null_terminate(string), do: <<string::binary, 0>>
end

defmodule Air.PsqlServer.Protocol.Messages do
  @moduledoc """
  Encoding and decoding of PostgreSQL protocol messages.

  Formats are described [here](https://www.postgresql.org/docs/9.6/static/protocol-message-formats.html).
  """

  alias Air.PsqlServer.Protocol

  @type server_message ::
    {:authentication_method, :cleartext} | :authentication_ok | :bind_complete | :close_complete |
    {:command_complete, String.t} | {:syntax_error, String.t} | {:fatal_error, String.t} | :ready_for_query |
    :parse_complete | :require_ssl |
    {:parameter_status, String.t, String.t} | {:parameter_description, [Protocol.psql_type]} |
    {:row_description, [Protocol.column], [:text | :binary]} |
    {:data_row, [Protocol.db_value], [Protocol.psql_type], [:text | :binary]}

  @type client_message_name ::
    :bind | :close | :describe | :execute | :flush | :parse | :password | :query | :sync | :terminate
  @type client_message_header :: %{type: client_message_name, length: non_neg_integer}


  #-----------------------------------------------------------------------------------------------------------
  # API functions
  #-----------------------------------------------------------------------------------------------------------

  @doc "Determines if the client message is an ssl_message."
  @spec ssl_message?(binary) :: boolean
  def ssl_message?(message), do:
    message == message_with_size(<<1234::16, 5679::16>>)

  @doc "Decodes the startup message."
  @spec decode_startup_message(<<_::64>>) ::
    %{length: non_neg_integer, version: %{major: non_neg_integer, minor: non_neg_integer}}
  def decode_startup_message(<<length::32, major::16, minor::16>> = message), do:
    %{length: length - byte_size(message), version: %{major: major, minor: minor}}

  @doc "Decodes the message header of the client message."
  @spec decode_message_header(<<_::40>>) :: client_message_header
  def decode_message_header(<<message_byte::8, length::32>>), do:
    %{type: client_message_name(message_byte), length: length - 4}

  @doc "Decodes the login_params message."
  @spec decode_login_params(binary) :: %{String.t => String.t}
  def decode_login_params(raw_login_params) do
    raw_login_params
    |> String.split(<<0>>)
    |> Stream.chunk(2)
    |> Stream.map(&List.to_tuple/1)
    |> Enum.into(%{})
  end

  @doc "Decodes the payload of the client message with the given name."
  @spec decode_message(client_message_name, binary) :: map
  def decode_message(:close, <<type, name::binary>>), do:
    %{type: type, name: hd(:binary.split(name, <<0>>))}
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
    param_types = for <<oid::32 <- parse_data>> do
      if oid == 0, do: :unknown, else: type_name(oid)
    end

    %{
      name: name,
      query: query,
      num_params: num_params,
      param_types: param_types,
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
  def encode_message(:bind_complete), do: server_message(:bind_complete, <<>>)
  def encode_message(:close_complete), do: server_message(:close_complete, <<>>)
  def encode_message({:command_complete, tag}), do: server_message(:command_complete, null_terminate(tag))
  def encode_message({:data_row, row, column_types, formats}), do:
    server_message(:data_row, <<length(row)::16, encode_row(row, column_types, formats)::binary>>)
  def encode_message({:syntax_error, error}), do: error_message("ERROR", "42601", error)
  def encode_message({:fatal_error, reason}), do: error_message("FATAL", "28000", reason)
  def encode_message(:ready_for_query), do: server_message(:ready_for_query, <<?I>>)
  def encode_message(:parse_complete), do: server_message(:parse_complete, <<>>)
  def encode_message(:require_ssl), do: <<?S>>
  def encode_message({:row_description, columns, result_codes}) do
    columns_descriptions =
      columns
      |> Enum.zip(Stream.cycle(result_codes))
      |> Enum.map(&column_description/1)
      |> IO.iodata_to_binary()

    server_message(:row_description, <<length(columns)::16, columns_descriptions::binary>>)
  end
  def encode_message({:parameter_status, name, value}), do:
    server_message(:parameter_status, null_terminate(name) <> null_terminate(value))
  def encode_message({:parameter_description, param_types}) do
    encoded_types =
      param_types
      |> Enum.map(&type_oid/1)
      |> Enum.map(&<<&1::32>>)
      |> IO.iodata_to_binary()

    server_message(:parameter_description, <<length(param_types)::16, encoded_types::binary>>)
  end
  def encode_message(:startup_message, major, minor, opts) do
    [
      <<major::16, minor::16>>,
      Enum.map(opts, fn({key, value}) -> null_terminate(to_string(key)) <> null_terminate(value) end)
    ]
    |> to_string()
    |> message_with_size()
  end

  @doc "Converts query parameters to proper Elixir types."
  @spec convert_params([any], [:text | :binary], [Protocol.psql_type]) :: [any]
  def convert_params(params, format_codes, param_types) do
    # per protocol, if param types are empty, all parameters are encoded as text
    param_types = if param_types == [], do: Enum.map(params, fn(_) -> :text end), else: param_types
    true = (length(params) == length(param_types))
    Enum.map(Enum.zip([format_codes, param_types, params]), &convert_param/1)
  end


  #-----------------------------------------------------------------------------------------------------------
  # Client (aka frontend) message headers
  #-----------------------------------------------------------------------------------------------------------

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
  # Server (i.e. "backend" as referenced in official PostgreSQL docs) messages
  #-----------------------------------------------------------------------------------------------------------

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
    defp server_message_byte(unquote(message_name)), do: unquote(message_byte)
  end

  defp server_message(message_name, payload), do:
    <<server_message_byte(message_name)::8, message_with_size(payload)::binary>>

  defp error_message(severity, code, message), do:
    server_message(:error_response, <<
      ?S, null_terminate(severity)::binary,
      ?C, null_terminate(code)::binary,
      ?M, null_terminate(message)::binary,
      0
    >>)


  #-----------------------------------------------------------------------------------------------------------
  # Types encoding
  #-----------------------------------------------------------------------------------------------------------

  for {type, meta} <- %{
    # Obtained as `select typname, oid, typlen from pg_type`
    boolean: %{oid: 16, len: 1},
    int2: %{oid: 21, len: 2},
    int4: %{oid: 23, len: 4},
    int8: %{oid: 20, len: 8},
    float4: %{oid: 700, len: 4},
    float8: %{oid: 701, len: 8},
    numeric: %{oid: 1700, len: -1},
    text: %{oid: 25, len: -1},
    date: %{oid: 1082, len: 4},
    time: %{oid: 1083, len: 8},
    timestamp: %{oid: 1114, len: 8},
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
  # Params conversion
  #-----------------------------------------------------------------------------------------------------------

  defp convert_param({_, _, nil}), do: nil
  defp convert_param({:text, type, value}), do: convert_text_param(type, value)
  defp convert_param({:binary, type, value}), do: normalize_postgrex_decoded_value(binary_decode(type, value))

  defp convert_text_param(:int2, param), do: String.to_integer(param)
  defp convert_text_param(:int4, param), do: String.to_integer(param)
  defp convert_text_param(:int8, param), do: String.to_integer(param)
  defp convert_text_param(:float4, value), do: String.to_float(value)
  defp convert_text_param(:float8, value), do: String.to_float(value)
  defp convert_text_param(:numeric, value), do: value |> Decimal.new() |> Decimal.to_float()
  defp convert_text_param(:boolean, "1"), do: true
  defp convert_text_param(:boolean, text), do: String.downcase(text) == "true"
  defp convert_text_param(:text, param) when is_binary(param), do: param
  defp convert_text_param(:unknown, param) when is_binary(param), do: param

  defp normalize_postgrex_decoded_value(%Decimal{} = value), do: Decimal.to_float(value)
  defp normalize_postgrex_decoded_value(value), do: value


  #-----------------------------------------------------------------------------------------------------------
  # Row encoding
  #-----------------------------------------------------------------------------------------------------------

  defp encode_row(row, column_types, formats), do:
    Enum.zip([Stream.cycle(formats), column_types, row])
    |> Enum.map(&encode_value/1)
    |> IO.iodata_to_binary()

  defp encode_value({_, _, nil}), do: <<-1::32>>
  defp encode_value({:text, _, value}), do: with_size(to_string(value))
  defp encode_value({:binary, type, value}), do:
    binary_encode(type, normalize_for_postgrex_encoding(type, value))

  defp with_size(encoded), do:
    <<byte_size(encoded)::32, encoded::binary>>

  defp normalize_for_postgrex_encoding(:numeric, value), do: Decimal.new(value)
  defp normalize_for_postgrex_encoding(:date, value), do: Date.from_iso8601!(value)
  defp normalize_for_postgrex_encoding(:time, value), do: Time.from_iso8601!(value)
  defp normalize_for_postgrex_encoding(:timestamp, value), do: NaiveDateTime.from_iso8601!(value)
  defp normalize_for_postgrex_encoding(_, value), do: value


  #-----------------------------------------------------------------------------------------------------------
  # Helpers for postgrex extensions
  #-----------------------------------------------------------------------------------------------------------

  # We're using postgrex extensions to encode/decode binary values. This is somewhat hacky, since the
  # interface is not documented, but it allows us to freely reuse a lot of logic.

  for {psql_type, extension, extension_arg} <- [
    {:int2, Int2, nil}, {:int4, Int4, nil}, {:int8, Int8, nil},
    {:float4, Float4, nil}, {:float8, Float8, nil},
    {:numeric, Numeric, nil},
    {:boolean, Bool, nil},
    {:date, Date, :elixir}, {:time, Time, :elixir}, {:timestamp, Timestamp, :elixir},
    {:text, Raw, :reference}
  ] do
    extension = Module.concat(Postgrex.Extensions, extension)

    defp binary_decode(unquote(psql_type), param) do
      case with_size(param), do: unquote(extension.decode(extension_arg))
    end

    defp binary_encode(unquote(psql_type), value) do
      case value, do: unquote(extension.encode(extension_arg))
    end
  end


  #-----------------------------------------------------------------------------------------------------------
  # Other helpers
  #-----------------------------------------------------------------------------------------------------------

  defp message_with_size(payload), do:
    <<(4 + byte_size(payload))::32, payload::binary>>

  defp null_terminate(string), do: <<string::binary, 0>>
end

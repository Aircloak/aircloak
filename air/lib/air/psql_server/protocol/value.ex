defmodule Air.PsqlServer.Protocol.Value do
  @moduledoc "Encoding to and decoding from raw PostgreSQL values."
  @types %{
    # Obtained as `select typname, oid, typlen from pg_type`
    boolean: %{oid: 16, len: 1, postgrex_extension: {Bool, nil}},
    char: %{oid: 18, len: 1, postgrex_extension: {Raw, :reference}},
    name: %{oid: 19, len: 64, postgrex_extension: {Name, :reference}},
    int8: %{oid: 20, len: 8, postgrex_extension: {Int8, nil}},
    int2: %{oid: 21, len: 2, postgrex_extension: {Int2, nil}},
    int4: %{oid: 23, len: 4, postgrex_extension: {Int4, nil}},
    regproc: %{oid: 24, len: 4},
    text: %{oid: 25, len: -1, postgrex_extension: {Raw, :reference}},
    oid: %{oid: 26, len: 4, postgrex_extension: {OID, nil}},
    float4: %{oid: 700, len: 4, postgrex_extension: {Float4, nil}},
    float8: %{oid: 701, len: 8, postgrex_extension: {Float8, nil}},
    unknown: %{oid: 705, len: -1, postgrex_extension: {Raw, :reference}},
    oidarray: %{oid: 1028, len: -1},
    bpchar: %{oid: 1042, len: -1, postgrex_extension: {Raw, :reference}},
    varchar: %{oid: 1043, len: -1, postgrex_extension: {Raw, :reference}},
    date: %{oid: 1082, len: 4, postgrex_extension: {Date, :elixir}},
    time: %{oid: 1083, len: 8, postgrex_extension: {Time, :elixir}},
    timestamp: %{oid: 1114, len: 8, postgrex_extension: {Timestamp, :elixir}},
    timestamptz: %{oid: 1184, len: 8, postgrex_extension: {TimestampTZ, :elixir}},
    timetz: %{oid: 1266, len: 12, postgrex_extension: {TimeTZ, :elixir}},
    numeric: %{oid: 1700, len: -1, postgrex_extension: {Numeric, nil}}
  }

  @type type ::
          unquote(
            @types
            |> Map.keys()
            |> Enum.reduce(nil, fn
              type, nil ->
                quote do
                  unquote(type)
                end

              type, acc ->
                quote do
                  unquote(type) | unquote(acc)
                end
            end)
          )

  @type format :: :text | :binary

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns the type atom from the given OID."
  @spec type_from_oid(integer) :: type
  for {type, meta} <- @types do
    def type_from_oid(unquote(meta.oid)), do: unquote(type)
  end

  @doc "Returns type information for the given type."
  @spec type_info(type) :: %{oid: integer, len: integer}
  for {type, meta} <- @types do
    def type_info(unquote(type)), do: unquote(Macro.escape(Map.delete(meta, :postgrex_extension)))
  end

  @doc "Encodes the value, using the provided encoding format and desired type."
  @spec encode(any, format, type) :: binary
  def encode(nil, _, _), do: <<-1::32>>

  def encode(value, :text, type),
    do:
      value
      |> text_encode(type)
      |> with_size()

  def encode(value, :binary, type),
    do:
      value
      |> normalize_for_postgrex_encoding(type)
      |> binary_encode(type)

  @doc "Decodes the value encoded with the given format and type."
  @spec decode(nil | binary, format, type) :: any
  def decode(nil, _, _), do: nil
  def decode(value, :text, type), do: text_decode(value, type)

  def decode(value, :binary, type),
    do:
      value
      |> binary_decode(type)
      |> normalize_postgrex_decoded_value()

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp text_encode(byte, :char), do: <<byte>>
  defp text_encode(oids, :oidarray), do: "{#{oids |> Stream.map(&to_string/1) |> Enum.join(",")}}"
  defp text_encode(value, _), do: to_string(value)

  defp text_decode(param, :int2), do: String.to_integer(param)
  defp text_decode(param, :int4), do: String.to_integer(param)
  defp text_decode(param, :int8), do: String.to_integer(param)
  defp text_decode(value, :float4), do: String.to_float(value)
  defp text_decode(value, :float8), do: String.to_float(value)
  defp text_decode(value, :numeric), do: value |> Decimal.new() |> Decimal.to_float()
  defp text_decode("1", :boolean), do: true
  defp text_decode(text, :boolean), do: String.downcase(text) == "true"
  defp text_decode(<<char>>, :char), do: char
  defp text_decode(param, :text) when is_binary(param), do: param
  defp text_decode(param, :name) when is_binary(param), do: param
  defp text_decode(param, :unknown) when is_binary(param), do: param

  defp normalize_for_postgrex_encoding(value, :numeric), do: Decimal.new(value)
  defp normalize_for_postgrex_encoding(value, :date), do: Date.from_iso8601!(value)
  defp normalize_for_postgrex_encoding(value, :time), do: Time.from_iso8601!(value)
  defp normalize_for_postgrex_encoding(value, :timestamp), do: NaiveDateTime.from_iso8601!(value)
  defp normalize_for_postgrex_encoding(value, _), do: value

  defp normalize_postgrex_decoded_value(%Decimal{} = value), do: Decimal.to_float(value)
  defp normalize_postgrex_decoded_value(value), do: value

  for {type, %{postgrex_extension: {extension, extension_arg}}} <- @types do
    extension = Module.concat(Postgrex.Extensions, extension)

    defp binary_decode(value, unquote(type)) do
      case with_size(value), do: unquote(extension.decode(extension_arg))
    end

    defp binary_encode(value, unquote(type)) do
      case value, do: unquote(extension.encode(extension_arg))
    end
  end

  defp with_size(encoded), do: <<byte_size(encoded)::32, encoded::binary>>
end

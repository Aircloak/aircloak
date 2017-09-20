defmodule Cloak.Query.DataDecoder do
  @moduledoc """
  Implements API for decoding individually encoded columns in a data store.
  The decoders need to be specified in the table configuration, for example:
  "decoders": [
    {"method": "base64", "columns": ["column1"]},
    {"method": "aes_cbc_128", "key": "1234567890ABCDEF", "columns": ["column1", "column2"]}
  ]
  """

  alias Cloak.Sql.{Query, Expression}
  alias Cloak.DataSource.Table

  @type type :: Table.data_type
  @type decoder :: (type -> {:ok, type} | :error)
  @type t :: %{method: decoder, columns: [String.t], in: type, out: type}


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Initializes the configured decoders from a loaded table."
  @spec init(Table.t) :: Table.t
  def init(table) do
    decoders = for decoder <- table.decoders do
      validate_columns(decoder.columns, table.columns)
      create_from_config(table, decoder)
    end
    columns = for column <- table.columns do
      decoded_type = Enum.reduce(decoders, column.type, &get_decoded_column_type(&1, column.name, &2))
      %{column | type: decoded_type}
    end
    %{table | decoders: decoders, columns: columns}
  end

  @doc "Decodes encoded data, if needed."
  @spec decode(Enumerable.t, Query.t) :: Enumerable.t
  def decode(stream, %Query{db_columns: db_columns}) do
    if Enum.any?(db_columns, &needs_decoding?/1) do
      decoders = Enum.map(db_columns, &get_decoders/1)
      Stream.map(stream, fn (row) ->
        Enum.zip(row, decoders) |> Enum.map(&decode_value/1)
      end)
    else
      stream
    end
  end

  @doc "Checks a column to see if it needs decoding in the cloak before usage."
  @spec needs_decoding?(Expression.t) :: boolean
  def needs_decoding?(%Expression{name: name, table: %{decoders: decoders}}), do:
    Enum.any?(decoders, &Enum.member?(&1.columns, name))
  def needs_decoding?(%Expression{}), do: false

  @doc "Returns the actual type of the column in the database."
  @spec encoded_type(Expression.t) :: type
  def encoded_type(%Expression{name: name, type: type, table: %{decoders: decoders}}) do
    decoders
    |> Enum.reverse()
    |> Enum.reduce(type, &get_encoded_column_type(&1, name, &2))
  end
  def encoded_type(column), do: column.type


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp validate_columns(decoded_columns, table_columns) do
    for name <- decoded_columns do
      unless Enum.find(table_columns, &match?(%{name: ^name}, &1)) != nil do
        raise "Encoded column `#{name}` doesn't exists."
      end
    end
  end

  defp create_from_config(table, %{method: method} = decoder), do:
    do_create_from_config(table, decoder) |> Map.put(:spec, method)

  defp do_create_from_config(_table, %{method: method} = decoder) when is_function(method), do: decoder
  defp do_create_from_config(_table, %{method: "base64", columns: columns}), do:
    %{method: &Base.decode64/1, columns: columns, in: :text, out: :text}
  defp do_create_from_config(table, %{method: "aes_cbc_128", key: key, columns: columns}) do
    if byte_size(key) != 16, do: raise "Decryption key from table `#{table.name}` has an invalid length."
    %{method: &aes_cbc128_decode(&1, key), columns: columns, in: :text, out: :text}
  end
  defp do_create_from_config(_table, %{method: "text_to_integer", columns: columns}), do:
    %{method: &text_to_integer/1, columns: columns, in: :text, out: :integer}
  defp do_create_from_config(_table, %{method: "text_to_real", columns: columns}), do:
    %{method: &text_to_real/1, columns: columns, in: :text, out: :real}
  defp do_create_from_config(_table, %{method: "text_to_datetime", columns: columns}), do:
    %{method: &text_to_datetime/1, columns: columns, in: :text, out: :datetime}
  defp do_create_from_config(_table, %{method: "text_to_date", columns: columns}), do:
    %{method: &text_to_date/1, columns: columns, in: :text, out: :date}
  defp do_create_from_config(_table, %{method: "text_to_time", columns: columns}), do:
    %{method: &text_to_time/1, columns: columns, in: :text, out: :time}
  defp do_create_from_config(_table, %{method: "text_to_boolean", columns: columns}), do:
    %{method: &text_to_boolean/1, columns: columns, in: :text, out: :boolean}
  defp do_create_from_config(_table, decoder), do:
    raise "Invalid data decoder definition: `#{inspect(decoder)}`."

  defp get_decoders(%Expression{table: :unknown}), do: []
  defp get_decoders(%Expression{name: name, table: table}), do:
    table.decoders
    |> Enum.filter(&Enum.member?(&1.columns, name))
    |> Enum.map(&(&1.method))


  defp get_decoded_column_type(decoder, name, type) do
    if Enum.member?(decoder.columns, name) do
      if decoder.in == type do
        decoder.out
      else
        raise "Invalid input type for column `#{name}`: expected #{decoder.in}, got #{type}."
      end
    else
      type
    end
  end

  defp get_encoded_column_type(decoder, name, type) do
    if Enum.member?(decoder.columns, name) do
      true = type == decoder.out
      decoder.in
    else
      type
    end
  end

  defp decode_value({nil, _}), do: nil
  defp decode_value({value, []}), do: value
  defp decode_value({value, [decoder | next]}) do
    case decoder.(value) do
      :error -> value
      {:ok, value} -> decode_value({value, next})
    end
  end

  @zero_iv String.duplicate(<<0>>, 16)
  defp aes_cbc128_decode("", _key), do: :error
  defp aes_cbc128_decode(value, _key) when rem(byte_size(value), 16) != 0, do: :error
  defp aes_cbc128_decode(value, key) do
    value = :crypto.block_decrypt(:aes_cbc128, key, @zero_iv, value)
    last = :binary.last(value)
    {value, padding} = String.split_at(value, -last)
    if padding == String.duplicate(<<last>>, last),
      do: {:ok, value},
      else: :error
  end

  defp text_to_integer(value) when is_binary(value) do
    case Integer.parse(value) do
      {value, ""} -> {:ok, value}
      :error -> :error
    end
  end

  defp text_to_real(value) when is_binary(value) do
    case Float.parse(value) do
      {value, ""} -> {:ok, value}
      :error -> :error
    end
  end

  defp text_to_datetime(value) when is_binary(value) do
    case Cloak.Time.parse_datetime(value) do
      {:ok, value} -> {:ok, value}
      {:error, _reason} -> :error
    end
  end

  defp text_to_date(value) when is_binary(value) do
    case Cloak.Time.parse_date(value) do
      {:ok, value} -> {:ok, value}
      {:error, _reason} -> :error
    end
  end

  defp text_to_time(value) when is_binary(value) do
    case Cloak.Time.parse_time(value) do
      {:ok, value} -> {:ok, value}
      {:error, _reason} -> :error
    end
  end

  defp text_to_boolean(value) when is_binary(value) do
    case String.downcase(value) do
      input when input in ["true", "yes", "1"] -> {:ok, true}
      input when input in ["false", "no", "0"] -> {:ok, false}
      _otherwise -> :error
    end
  end
  defp text_to_boolean(_), do: :error
end

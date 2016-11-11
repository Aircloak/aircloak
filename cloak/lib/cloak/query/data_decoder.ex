defmodule Cloak.Query.DataDecoder do
  @moduledoc "Implements API for decoding individually encoded columns in a data store."

  alias Cloak.Aql.{Query, Column}

  @type decoder :: (String.t -> {:ok, String.t} | :error)
  @type t :: %{method: decoder, columns: [String.t]}


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Initializes the configured decoders from a loaded table."
  @spec init(Cloak.DataSource.table) :: Cloak.DataSource.table
  def init(table) do
    decoders = for decoder <- table.decoders do
      validate_columns(decoder.columns, table.columns)
      create_from_config(decoder)
    end
    %{table | decoders: decoders}
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
  @spec needs_decoding?(Column.t) :: boolean
  def needs_decoding?(%Column{name: name, table: %{decoders: decoders}}), do:
    Enum.any?(decoders, &Enum.member?(&1.columns, name))
  def needs_decoding?(%Column{}), do: false


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp validate_columns(decoded_columns, table_columns) do
    for name <- decoded_columns do
      unless Enum.member?(table_columns, {name, :text}) do
        raise "Column `#{name}` is not a valid target for decoding (either is missing or is not of type text)."
      end
    end
  end

  defp create_from_config(%{method: "base64", columns: columns}), do:
    %{method: &Base.decode64/1, columns: columns}
  defp create_from_config(%{method: "aes_cbc_128", key: key, columns: columns}), do:
    %{method: &aes_cbc128_decode(&1, key), columns: columns}
  defp create_from_config(decoder), do:
    raise "Invalid data decoder definition: `#{inspect(decoder)}`."

  defp get_decoders(%Column{name: name, table: table}) do
    table.decoders
    |> Enum.map(&if Enum.member?(&1.columns, name), do: &1.method, else: nil)
    |> Enum.reject(& &1 == nil)
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
  defp aes_cbc128_decode(value, _key) when rem(byte_size(value), 16) != 0, do: :error
  defp aes_cbc128_decode(value, key) do
    value = :crypto.block_decrypt(:aes_cbc128, key, @zero_iv, value)
    <<last>> = String.last(value)
    {value, padding} = String.split_at(value, -last)
    if padding == String.duplicate(<<last>>, last),
      do: {:ok, value},
      else: :error
  end
end

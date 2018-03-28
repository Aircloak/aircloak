defmodule Cloak.DataSource.MongoDB.Schema do
  @moduledoc "MongoDB helper functions for detecting and mapping the schema of a collection."

  alias Cloak.DataSource.Table

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Maps the list of field names in a collection to a set of tables."
  @spec build([{String.t(), Table.data_type()}], Table.t()) :: [Table.t()]
  def build(fields, table) do
    fields
    |> Enum.map(fn {name, type} ->
      {String.split(name, "[]"), type}
    end)
    |> Enum.reduce(%{}, &build_schema/2)
    |> build_tables(Map.put(table, :array_path, []))
  end

  @doc "Checks to see if the column name refers to the size of an array."
  @spec is_array_size?(String.t()) :: boolean
  def is_array_size?(name) when is_binary(name), do: String.contains?(name, "#")

  @doc "Returns the name of the MongoDB array mapped to the given virtual column."
  @spec array_size_field(String.t()) :: String.t()
  def array_size_field(name) when is_binary(name), do: String.replace(name, "#", "")

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp schema_append(schema, key, value), do: Map.update(schema, key, [value], &(&1 ++ [value]))

  defp build_schema({[name], type}, schema), do: schema_append(schema, :base, {name, type})

  defp build_schema({[array, ""], type}, schema),
    do:
      schema
      |> schema_append(:base, {array <> "#", :integer})
      |> Map.put(array, %{})
      |> Map.put(array, type)

  defp build_schema({[array | name], type}, schema) do
    schema =
      case schema[array] do
        nil ->
          schema
          |> schema_append(:base, {array <> "#", :integer})
          |> Map.put(array, %{})

        _ ->
          schema
      end

    array_schema = build_schema({name, type}, schema[array])
    Map.put(schema, array, array_schema)
  end

  defp build_tables(_schema, _table, _parent_columns \\ [])

  defp build_tables(%{} = schema, table, parent_columns) do
    columns =
      Enum.reject(parent_columns, &is_array_size?(&1.name)) ++
        Enum.map(schema.base, fn {name, type} ->
          Table.column(to_string(table.array_path) <> name, type)
        end)

    arrays = Map.keys(schema) -- [:base]

    array_tables =
      for array <- arrays do
        decoders = reject_array_decoders(table.decoders, table.array_path, arrays -- [array])

        array_table = %{
          table
          | array_path: table.array_path ++ [array],
            decoders: decoders,
            name: build_table_name(table.name, array),
            db_name: build_table_name(table.db_name, array)
        }

        build_tables(schema[array], array_table, columns)
      end

    decoders = reject_array_decoders(table.decoders, table.array_path, arrays)
    [%{table | columns: columns, decoders: decoders} | List.flatten(array_tables)]
  end

  defp build_tables(type, table, parent_columns) do
    columns = Enum.reject(parent_columns, &is_array_size?(&1.name)) ++ [Table.column(to_string(table.array_path), type)]

    [%{table | columns: columns}]
  end

  defp build_table_name(parent, "." <> array), do: build_table_name(parent, array)
  defp build_table_name(parent, array), do: "#{parent}_#{array}"

  defp reject_array_decoders(decoders, path, arrays) do
    arrays = for array <- arrays, do: to_string(path ++ [array])
    prefixes = for array <- arrays, do: array <> "."

    decoders
    |> Enum.map(fn decoder ->
      %{
        decoder
        | columns:
            Enum.reject(
              decoder.columns,
              &(String.starts_with?(&1, prefixes) || Enum.member?(arrays, &1))
            )
      }
    end)
    |> Enum.reject(&Enum.empty?(&1.columns))
  end
end

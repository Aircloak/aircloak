defmodule Compliance.TableDefinitions do
  @moduledoc false

  alias Compliance.Data

  @type column_type :: :integer | :real | :boolean | :text | :datetime

  @doc "Returns the table definition for the normal table."
  @spec plain() :: Map.t
  def plain(), do:
    raw_table_definitions()
    |> Enum.map(fn({table, %{columns: raw_columns} = definitions}) ->
      columns = raw_columns
      |> Enum.map(fn
        ({_name, _type} = column) -> column
        ({name, type, _decoders}) -> {name, type}
      end)
      {table, Map.put(definitions, :columns, columns)}
    end)
    |> Enum.into(%{})

  @doc "Returns the table definition for the encoded table."
  @spec encoded() :: Map.t
  def encoded(), do:
    raw_table_definitions()
    |> Enum.map(fn({table, %{columns: raw_columns} = definitions}) ->
      {columns, decoders} = raw_columns
      |> Enum.reduce({[], []}, fn
        ({_name, _type} = plain_column, {columns_acc, decoders_acc}) ->
          {[plain_column | columns_acc], decoders_acc}
        ({name, _type, [{:decoders, decoders}]}, {columns_acc, decoders_acc}) ->
          executable_decoders = decoders
          |> Enum.map(fn
            (decoder) when is_atom(decoder) -> %{method: Atom.to_string(decoder), columns: [name]}
            ({decoder, options}) ->
              Enum.reduce(options, %{method: Atom.to_string(decoder), columns: [name]}, fn({key, value}, acc) ->
                Map.put(acc, key, value)
              end)
          end)
          {[{name, :text} | columns_acc], executable_decoders ++ decoders_acc}
      end)
      updated_definitions = definitions
      |> Map.put(:columns, columns)
      |> Map.put(:decoders, decoders)
      {table, updated_definitions}
    end)
    |> Enum.into(%{})

  @doc "Returns a defining containing information about how the UID-column is defined or derived"
  @spec uid_definitions() :: Map.t
  def uid_definitions() do
    %{
      users: %{
        user_id: "user_id",
      },
      addresses: %{
        projection: %{
          table: "users",
          foreign_key: "user_fk",
          primary_key: "id",
          user_id_alias: "uid",
        },
      },
      notes: %{
        projection: %{
          table: "users",
          foreign_key: "user_fk",
          primary_key: "id",
          user_id_alias: "uid",
        },
      },
      notes_changes: %{
        projection: %{
          table: "notes",
          foreign_key: "note_id",
          primary_key: "id",
          user_id_alias: "uid",
        },
      },
    }
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp raw_table_definitions() do
    %{
      users: %{
        columns: [
          {"id", :integer},
          {"user_id", :integer},
          {"age", :integer, decoders: [:text_to_integer]},
          {"height", :real, decoders: [:text_to_real]},
          {"active", :boolean, decoders: [:text_to_boolean]},
          {"name", :text, decoders: [:base64, aes_cbc_128: [key: Data.encryption_key()]]},
        ],
      },
      addresses: %{
        columns: [
          {"user_fk", :integer},
          {"home.city", :text, decoders: [:base64, aes_cbc_128: [key: Data.encryption_key()]]},
          {"work.city", :text, decoders: [:base64, aes_cbc_128: [key: Data.encryption_key()]]},
          {"home.postal_code", :integer, decoders: [:text_to_integer]},
          {"work.postal_code", :integer, decoders: [:text_to_integer]},
        ],
      },
      notes: %{
        columns: [
          {"user_fk", :integer},
          {"id", :integer},
          {"title", :text, decoders: [:base64, aes_cbc_128: [key: Data.encryption_key()]]},
          {"content", :text, decoders: [:base64, aes_cbc_128: [key: Data.encryption_key()]]},
        ],
      },
      notes_changes: %{
        columns: [
          {"id", :integer},
          {"user_fk", :integer},
          {"note_id", :integer},
          {"title", :text, decoders: [:base64, aes_cbc_128: [key: Data.encryption_key()]]},
          {"content", :text, decoders: [:base64, aes_cbc_128: [key: Data.encryption_key()]]},
          {"changes.change", :text, decoders: [:base64, aes_cbc_128: [key: Data.encryption_key()]]},
          {"changes.date", :datetime, decoders: [:text_to_datetime]},
        ],
      },
    }
  end
end

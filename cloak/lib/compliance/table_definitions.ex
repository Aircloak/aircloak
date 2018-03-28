defmodule Compliance.TableDefinitions do
  @moduledoc false

  alias Compliance.Data

  @type column_type :: :integer | :real | :boolean | :text | :datetime

  @doc "Returns the table definition for the normal table."
  @spec plain(boolean) :: Map.t()
  def plain(produce_collections_rather_than_tables),
    do:
      raw_table_definitions()
      |> prepare_as_collections(produce_collections_rather_than_tables)
      |> Enum.map(fn {table, %{columns: raw_columns} = definitions} ->
        columns = Enum.map(raw_columns, fn {name, %{type: type}} -> {Atom.to_string(name), type} end)

        {table, Map.put(definitions, :columns, columns)}
      end)
      |> Enum.into(%{})

  @doc "Returns the table definition for the encoded table."
  @spec encoded(boolean) :: Map.t()
  def encoded(produce_collections_rather_than_tables),
    do:
      raw_table_definitions()
      |> prepare_as_collections(produce_collections_rather_than_tables)
      |> Enum.map(fn {table, %{columns: raw_columns} = definitions} ->
        {columns, decoders} =
          raw_columns
          |> Enum.reduce({[], []}, fn
            {name, %{decoders: decoders}}, {columns_acc, decoders_acc} ->
              name = Atom.to_string(name)

              executable_decoders =
                decoders
                |> Enum.map(fn
                  decoder when is_atom(decoder) ->
                    %{method: Atom.to_string(decoder), columns: [name]}

                  {decoder, options} ->
                    Enum.reduce(options, %{method: Atom.to_string(decoder), columns: [name]}, fn {key, value}, acc ->
                      Map.put(acc, key, value)
                    end)
                end)

              {[{name, :text} | columns_acc], executable_decoders ++ decoders_acc}

            {name, %{type: type}}, {columns_acc, decoders_acc} ->
              {[{Atom.to_string(name), type} | columns_acc], decoders_acc}
          end)

        updated_definitions =
          definitions
          |> Map.put(:columns, columns)
          |> Map.put(:decoders, decoders)

        {table, updated_definitions}
      end)
      |> Enum.into(%{})

  @doc "Returns a defining containing information about how the UID-column is defined or derived"
  @spec uid_definitions() :: Map.t()
  def uid_definitions() do
    %{
      users: %{
        user_id: "user_id"
      },
      addresses: %{
        projection: %{
          table: "users",
          foreign_key: "user_fk",
          primary_key: "id",
          user_id_alias: "uid"
        }
      },
      notes: %{
        projection: %{
          table: "users",
          foreign_key: "user_fk",
          primary_key: "id",
          user_id_alias: "uid"
        }
      },
      notes_changes: %{
        projection: %{
          table: "notes",
          foreign_key: "note_id",
          primary_key: "id",
          user_id_alias: "uid"
        }
      }
    }
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp prepare_as_collections(collections, false), do: collections

  defp prepare_as_collections(collections, true) do
    {primary_collections, sub_collections} =
      Enum.split_with(collections, fn {_name, definition} ->
        is_nil(definition[:sub_collection_of])
      end)

    primary_collections = Enum.into(primary_collections, %{})

    Enum.reduce(sub_collections, primary_collections, fn {_name, sub_collection}, primaries_acc ->
      primary_name = sub_collection[:sub_collection_of]
      primary_collection = primaries_acc[primary_name]

      primary_columns =
        Enum.reduce(sub_collection.columns, primary_collection.columns, fn {name, definition}, acc ->
          if is_nil(Map.get(acc, name)) do
            Map.put(acc, name, definition)
          else
            acc
          end
        end)

      updated_primary_collection = Map.put(primary_collection, :columns, primary_columns)
      Map.put(primaries_acc, primary_name, updated_primary_collection)
    end)
  end

  defp raw_table_definitions() do
    %{
      users: %{
        columns: %{
          id: %{type: :integer},
          user_id: %{type: :integer},
          age: %{type: :integer, decoders: [:text_to_integer]},
          height: %{type: :real, decoders: [:text_to_real]},
          active: %{type: :boolean, decoders: [:text_to_boolean]},
          name: %{type: :text, decoders: [:base64, aes_cbc_128: [key: Data.encryption_key()]]},
          nullable: %{type: :real, decoders: [:text_to_real]}
        }
      },
      addresses: %{
        columns: %{
          user_fk: %{type: :integer},
          "home.city": %{
            type: :text,
            decoders: [:base64, aes_cbc_128: [key: Data.encryption_key()]]
          },
          "work.city": %{
            type: :text,
            decoders: [:base64, aes_cbc_128: [key: Data.encryption_key()]]
          },
          "home.postal_code": %{type: :integer, decoders: [:text_to_integer]},
          "work.postal_code": %{type: :integer, decoders: [:text_to_integer]}
        }
      },
      notes: %{
        columns: %{
          user_fk: %{type: :integer},
          id: %{type: :integer},
          title: %{type: :text, decoders: [:base64, aes_cbc_128: [key: Data.encryption_key()]]},
          content: %{type: :text, decoders: [:base64, aes_cbc_128: [key: Data.encryption_key()]]}
        }
      },
      notes_changes: %{
        # I.e. this collection is really part of notes if considering it as a document
        sub_collection_of: :notes,
        columns: %{
          id: %{type: :integer},
          note_id: %{type: :integer},
          title: %{type: :text, decoders: [:base64, aes_cbc_128: [key: Data.encryption_key()]]},
          content: %{type: :text, decoders: [:base64, aes_cbc_128: [key: Data.encryption_key()]]},
          "changes.change": %{
            type: :text,
            decoders: [:base64, aes_cbc_128: [key: Data.encryption_key()]]
          },
          "changes.date": %{type: :datetime, decoders: [:text_to_datetime]}
        }
      }
    }
  end
end

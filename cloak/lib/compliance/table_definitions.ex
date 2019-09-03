defmodule Compliance.TableDefinitions do
  @moduledoc false

  alias Compliance.Data

  @type column_type :: :integer | :real | :boolean | :text | :datetime | :date

  @doc "Returns the table definition for the normal table."
  @spec plain() :: Map.t()
  def plain(),
    do:
      raw_table_definitions()
      |> Enum.map(fn {table, %{columns: raw_columns} = definitions} ->
        columns = Enum.map(raw_columns, fn {name, %{type: type}} -> {Atom.to_string(name), type} end)

        {table, Map.put(definitions, :columns, columns)}
      end)
      |> Enum.into(%{})

  @doc "Returns the table definition for the encoded table."
  @spec encoded() :: Map.t()
  def encoded(),
    do:
      raw_table_definitions()
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
          nullable: %{type: :real, decoders: [:text_to_real]},
          birthday: %{type: :date},
          column_with_a_very_long_name: %{type: :text}
        },
        keys: %{
          "user_id" => :user_id,
          "id" => :user_fk
        },
        user_id: "user_id"
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
        },
        keys: %{
          "user_fk" => :user_fk
        }
      },
      notes: %{
        columns: %{
          user_fk: %{type: :integer},
          id: %{type: :integer},
          title: %{type: :text, decoders: [:base64, aes_cbc_128: [key: Data.encryption_key()]]},
          content: %{type: :text, decoders: [:base64, aes_cbc_128: [key: Data.encryption_key()]]}
        },
        keys: %{
          "user_fk" => :user_fk,
          "id" => :note_id
        }
      },
      notes_changes: %{
        columns: %{
          id: %{type: :integer},
          note_id: %{type: :integer},
          title: %{type: :text, decoders: [:base64, aes_cbc_128: [key: Data.encryption_key()]]},
          content: %{type: :text, decoders: [:base64, aes_cbc_128: [key: Data.encryption_key()]]},
          change: %{
            type: :text,
            decoders: [:base64, aes_cbc_128: [key: Data.encryption_key()]]
          },
          date: %{type: :datetime, decoders: [:text_to_datetime]}
        },
        keys: %{
          "note_id" => :note_id
        }
      },
      users_public: %{
        content_type: :public,
        db_name: "users",
        columns: %{
          id: %{type: :integer},
          user_id: %{type: :integer},
          age: %{type: :integer, decoders: [:text_to_integer]},
          height: %{type: :real, decoders: [:text_to_real]},
          active: %{type: :boolean, decoders: [:text_to_boolean]},
          name: %{type: :text, decoders: [:base64, aes_cbc_128: [key: Data.encryption_key()]]},
          nullable: %{type: :real, decoders: [:text_to_real]},
          birthday: %{type: :date},
          column_with_a_very_long_name: %{type: :text}
        }
      }
    }
  end
end

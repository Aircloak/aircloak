defmodule Compliance.TableDefinitions do
  @moduledoc false

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
  @spec encoded(String.t()) :: Map.t()
  def encoded(suffix),
    do:
      raw_table_definitions()
      |> Enum.map(fn {table, %{columns: raw_columns} = definitions} ->
        {columns, select} =
          raw_columns
          |> Enum.reduce({[], []}, fn
            {name, %{decoder: decoder}}, {columns_acc, select_acc} ->
              name = Atom.to_string(name)
              select = "#{translate_decoder(decoder, name)} as #{name}"
              {[{name, :text} | columns_acc], [select | select_acc]}

            {name, %{type: type}}, {columns_acc, select_acc} ->
              {[{Atom.to_string(name), type} | columns_acc], [Atom.to_string(name) | select_acc]}
          end)

        query = "select #{Enum.join(select, ", ")} from #{Map.get(definitions, :db_name, table)}#{suffix}"

        updated_definitions =
          definitions
          |> Map.put(:columns, columns)
          |> Map.put(:query, query)

        {table, updated_definitions}
      end)
      |> Enum.into(%{})

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp raw_table_definitions() do
    %{
      users: %{
        columns: %{
          id: %{type: :integer},
          user_id: %{type: :integer},
          age: %{type: :integer, decoder: :text_to_integer},
          height: %{type: :real, decoder: :text_to_real},
          active: %{type: :boolean, decoder: :text_to_boolean},
          name: %{type: :text, decoder: :base64},
          nullable: %{type: :real, decoder: :text_to_real},
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
          "home.city": %{type: :text, decoder: :base64},
          "work.city": %{type: :text, decoder: :base64},
          "home.postal_code": %{type: :integer, decoder: :text_to_integer},
          "work.postal_code": %{type: :integer, decoder: :text_to_integer}
        },
        keys: %{
          "user_fk" => :user_fk
        }
      },
      notes: %{
        columns: %{
          user_fk: %{type: :integer},
          id: %{type: :integer},
          title: %{type: :text, decoder: :base64},
          content: %{type: :text, decoder: :base64}
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
          title: %{type: :text, decoder: :base64},
          content: %{type: :text, decoder: :base64},
          change: %{type: :text, decoder: :base64},
          date: %{type: :datetime, decoder: :text_to_datetime}
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
          age: %{type: :integer, decoder: :text_to_integer},
          height: %{type: :real, decoder: :text_to_real},
          active: %{type: :boolean, decoder: :text_to_boolean},
          name: %{type: :text, decoder: :base64},
          nullable: %{type: :real, decoder: :text_to_real},
          birthday: %{type: :date},
          column_with_a_very_long_name: %{type: :text}
        }
      }
    }
  end

  defp translate_decoder(:text_to_integer, column), do: "CAST(#{column} AS integer)"
  defp translate_decoder(:text_to_real, column), do: "CAST(#{column} AS real)"
  defp translate_decoder(:text_to_datetime, column), do: "CAST(#{column} AS datetime)"
  defp translate_decoder(:text_to_date, column), do: "CAST(#{column} AS date)"
  defp translate_decoder(:text_to_boolean, column), do: "CAST(#{column} AS boolean)"
  defp translate_decoder(:base64, column), do: "dec_b64(#{column})"
end

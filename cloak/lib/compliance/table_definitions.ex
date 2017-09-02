defmodule Compliance.TableDefinitions do
  @moduledoc false

  alias Compliance.Data

  @type column_type :: :integer | :real | :boolean | :text | :datetime

  @doc "Returns the table definition for the normal table."
  @spec plain() :: Map.t
  def plain() do
    %{
      users: %{
        columns: [
          {"id", :integer},
          {"user_id", :integer},
          {"age", :integer},
          {"height", :real},
          {"active", :boolean},
          {"name", :text},
        ],
      },
      addresses: %{
        columns: [
          {"user_fk", :integer},
          {"home.city", :text},
          {"work.city", :text},
          {"home.postal_code", :integer},
          {"work.postal_code", :integer},
        ],
      },
      notes: %{
        columns: [
          {"user_fk", :integer},
          {"id", :integer},
          {"title", :text},
          {"content", :text},
        ],
      },
      notes_changes: %{
        columns: [
          {"id", :integer},
          {"user_fk", :integer},
          {"note_id", :integer},
          {"title", :text},
          {"content", :text},
          {"changes.change", :text},
          {"changes.date", :datetime},
        ],
      },
    }
  end

  @doc "Returns the table definition for the encoded table."
  @spec encoded() :: Map.t
  def encoded() do
    %{
      users: %{
        columns: [
          {"id", :integer},
          {"user_id", :integer},
          {"age", :text},
          {"height", :text},
          {"active", :text},
          {"name", :text},
        ],
        decoders: [
          %{method: "base64", columns: ["name"]},
          %{method: "aes_cbc_128", key: Data.encryption_key(), columns: ["name"]},
          %{method: "text_to_integer", columns: ["age"]},
          %{method: "text_to_real", columns: ["height"]},
          %{method: "text_to_boolean", columns: ["active"]}
        ],
      },
      addresses: %{
        columns: [
          {"user_fk", :integer},
          {"home.city", :text},
          {"work.city", :text},
          {"home.postal_code", :text},
          {"work.postal_code", :text},
        ],
        decoders: [
          %{method: "base64", columns: ["home.city", "work.city"]},
          %{method: "aes_cbc_128", key: Data.encryption_key(), columns: ["home.city", "work.city"]},
          %{method: "text_to_integer", columns: ["home.postal_code", "work.postal_code"]}
        ],
      },
      notes: %{
        columns: [
          {"user_fk", :integer},
          {"id", :integer},
          {"title", :text},
          {"content", :text},
        ],
        decoders: [
          %{method: "base64", columns: ["title", "content"]},
          %{method: "aes_cbc_128", key: Data.encryption_key(), columns: ["title", "content"]}
        ],
      },
      notes_changes: %{
        columns: [
          {"id", :integer},
          {"user_fk", :integer},
          {"note_id", :integer},
          {"title", :text},
          {"content", :text},
          {"changes.change", :text},
          {"changes.date", :text},
        ],
        decoders: [
          %{method: "base64", columns: ["changes.change", "title", "content"]},
          %{method: "aes_cbc_128", key: Data.encryption_key(), columns: ["changes.change", "title", "content"]},
          %{method: "text_to_datetime", columns: ["changes.date"]}
        ],
      },
    }
  end

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
end

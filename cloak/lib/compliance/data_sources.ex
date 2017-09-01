defmodule Compliance.DataSources do
  @moduledoc false

  alias Compliance.Data


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  def get_config(), do: Map.get(read_config(), "data_sources")

  def create(prefix) do
    params = [
      {:user_id, "user_id"},
      {:add_user_id, false},
    ]
    Enum.each(Cloak.DataSource.all(), &create_table(&1.name, prefix, :users, [{:data_source, &1} | params]))

    params = [
      {:add_user_id, false},
      {:projection, %{
        table: "#{prefix}users",
        foreign_key: "user_fk",
        primary_key: "id",
        user_id_alias: "uid",
      }},
    ]
    Enum.each(Cloak.DataSource.all(), &create_table(&1.name, prefix, :addresses, [{:data_source, &1} | params]))

    params = [
      {:add_user_id, false},
      {:projection, %{
        table: "#{prefix}users",
        foreign_key: "user_fk",
        primary_key: "id",
        user_id_alias: "uid",
      }},
    ]
    Enum.each(Cloak.DataSource.all(), &create_table(&1.name, prefix, :notes, [{:data_source, &1} | params]))

    params = [
      {:add_user_id, false},
      {:projection, %{
        table: "#{prefix}notes",
        foreign_key: "note_id",
        primary_key: "id",
        user_id_alias: "uid",
      }},
    ]
    Enum.each(Cloak.DataSource.all(), &create_table(&1.name, prefix, :notes_changes, [{:data_source, &1} | params]))
  end

  def insert_data(prefix) do
    {nested_normal, nested_encoded} = Data.generate()

    normal = Data.flatten(nested_normal)
    encoded = Data.flatten(nested_encoded)

    Enum.each(Cloak.DataSource.all(), &insert_data(&1, prefix, :users, normal, encoded))
    Enum.each(Cloak.DataSource.all(), &insert_data(&1, prefix, :addresses, normal, encoded))
    Enum.each(Cloak.DataSource.all(), &insert_data(&1, prefix, :notes, normal, encoded))
    Enum.each(Cloak.DataSource.all(), &insert_data(&1, prefix, :notes_changes, normal, encoded))
  end


  # -------------------------------------------------------------------
  # Internal functions - Creating tables
  # -------------------------------------------------------------------

  # Postres Normal
  defp create_table("postgres_normal", prefix, :users, params), do:
    :ok = Cloak.Test.DB.create_table("#{prefix}users", "id integer, user_id integer, age integer, " <>
      "height float, active boolean, name text", params)
  defp create_table("postgres_normal", prefix, :addresses, params), do:
    :ok = Cloak.Test.DB.create_table("#{prefix}addresses", "user_fk integer, \"home.city\" text, " <>
      "\"home.postal_code\" integer, \"work.city\" text, \"work.postal_code\" integer", params)
  defp create_table("postgres_normal", prefix, :notes, params), do:
    :ok = Cloak.Test.DB.create_table("#{prefix}notes", "user_fk integer, id integer, title text, " <>
      "content text", params)
  defp create_table("postgres_normal", prefix, :notes_changes, params), do:
    :ok = Cloak.Test.DB.create_table("#{prefix}notes_changes", "user_fk integer, id integer, note_id integer, " <>
      "title text, content text, \"changes.date\" timestamp, \"changes.change\" text", params)

  # Postgres Encoded
  defp create_table("postgres_encoded", prefix, :users, params), do:
    :ok = Cloak.Test.DB.create_table("#{prefix}users", "id integer, user_id integer, age text, " <>
      "height text, active text, name text", extend_with_decoders(:users, params))
  defp create_table("postgres_encoded", prefix, :addresses, params), do:
    :ok = Cloak.Test.DB.create_table("#{prefix}addresses", "user_fk integer, \"home.city\" text, " <>
      "\"home.postal_code\" text, \"work.city\" text, \"work.postal_code\" text",
      extend_with_decoders(:addresses, params))
  defp create_table("postgres_encoded", prefix, :notes, params), do:
    :ok = Cloak.Test.DB.create_table("#{prefix}notes", "user_fk integer, id integer, title text, " <>
      "content text", extend_with_decoders(:notes, params))
  defp create_table("postgres_encoded", prefix, :notes_changes, params), do:
    :ok = Cloak.Test.DB.create_table("#{prefix}notes_changes", "user_fk integer, id integer, note_id integer, " <>
      "title text, content text, \"changes.date\" text, \"changes.change\" text",
      extend_with_decoders(:notes_changes, params))

  # Fallback
  defp create_table(data_source_name, _, table_name, _params), do:
    raise "Missing handler for creating table #{table_name} in data source: #{data_source_name}"


  # -------------------------------------------------------------------
  # Internal functions - Inserting data
  # -------------------------------------------------------------------

  defp insert_data(%{name: "postgres_normal"} = data_source, prefix, table, data, _encoded_data), do:
    perform_insert(data_source, prefix, table, data[table])

  defp insert_data(%{name: "postgres_encoded"} = data_source, prefix, table, _data, encoded_data), do:
    perform_insert(data_source, prefix, table, encoded_data[table])

  # Fallback
  defp insert_data(data_source_name, _, table_name, _data, _encoded_data), do:
    raise "Missing handler for inserting data for table #{table_name} in data source: #{data_source_name}"


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp extend_with_decoders(:users, params) do
    [
      {:decoders, [
        %{method: "base64", columns: ["name"]},
        %{method: "aes_cbc_128", key: Data.encryption_key(), columns: ["name"]},
        %{method: "text_to_integer", columns: ["age"]},
        %{method: "text_to_real", columns: ["height"]},
        %{method: "text_to_boolean", columns: ["active"]}
      ]} | params
    ]
  end
  defp extend_with_decoders(:notes, params) do
    [
      {:decoders, [
        %{method: "base64", columns: ["title", "content"]},
        %{method: "aes_cbc_128", key: Data.encryption_key(), columns: ["title", "content"]}
      ]} | params
    ]
  end
  defp extend_with_decoders(:notes_changes, params) do
    [
      {:decoders, [
        %{method: "base64", columns: ["changes.change", "title", "content"]},
        %{method: "aes_cbc_128", key: Data.encryption_key(), columns: ["changes.change", "title", "content"]},
        %{method: "text_to_datetime", columns: ["changes.date"]}
      ]} | params
    ]
  end
  defp extend_with_decoders(:addresses, params) do
    [
      {:decoders, [
        %{method: "base64", columns: ["home.city", "work.city"]},
        %{method: "aes_cbc_128", key: Data.encryption_key(), columns: ["home.city", "work.city"]},
        %{method: "text_to_integer", columns: ["home.postal_code", "work.postal_code"]}
      ]} | params
    ]
  end

  defp perform_insert(data_source, prefix, table, data) do
    column_names = data
    |> hd()
    |> Map.keys()
    |> Enum.sort()
    rows = data
    |> Enum.map(fn(entry) ->
      Enum.map(column_names, & Map.get(entry, &1))
    end)
    column_names = column_names
    |> Enum.map(& Atom.to_string/1)
    |> Enum.map(fn(name) ->
      if name =~ ~r/\./ do
        "\"#{name}\""
      else
        name
      end
    end)
    Cloak.Test.DB.insert_data("#{prefix}#{table}", column_names, rows, data_source)
  end

  defp read_config(), do:
    config_file_path()
    |> File.read!()
    |> Poison.decode!()

  defp config_file_path(), do:
    Path.join([Application.app_dir(:cloak, "priv"), "config", config_file_name()])

  defp config_file_name() do
    if System.get_env("TRAVIS") do
      "compliance_travis.json"
    else
      "compliance.json"
    end
  end
end

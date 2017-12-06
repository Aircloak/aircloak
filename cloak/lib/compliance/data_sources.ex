defmodule Compliance.DataSources do
  @moduledoc false

  alias Compliance.{Data, TableDefinitions}

  @normal_name_postfix ""
  @encoded_name_postfix "_encoded"


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Creates configured data sources from a configuration file"
  @spec all_from_config_initialized(String.t) :: [Cloak.DataSource.t]
  def all_from_config_initialized(name), do:
    name
    |> all_from_config()
    |> complete_data_source_definitions()
    |> Enum.map(&Cloak.DataSource.add_tables/1)

  @doc "Creates configured data sources from a configuration file without attempting to connect to the dataources"
  @spec all_from_config(String.t) :: [Cloak.DataSource.t]
  def all_from_config(name), do:
    read_config(name)["data_sources"]
    |> Cloak.DataSource.Utility.load_individual_data_source_configs()
    |> Enum.uniq_by(& {&1["parameters"], &1["driver"]})
    |> Cloak.DataSource.config_to_datasources()

  @doc "Creates tables for a normal and a encoded dataset and inserts data into them."
  @spec setup([DataSource.t], Map.t) :: :ok
  def setup(data_sources, data), do:
    data_sources
    |> Enum.map(&Task.async(fn -> setup_datasource(&1, data) end))
    |> Enum.each(&Task.await(&1, :timer.minutes(5)))

  @doc "Takes a rawling data source definition and expands it with table definitions"
  @spec complete_data_source_definitions([DataSource.t]) :: [DataSource.t]
  def complete_data_source_definitions(data_sources), do:
    expand_and_add_table_definitions(data_sources)

  @doc "Returns a data source config as JSON"
  @spec read_config(String.t) :: Map.t
  def read_config(name), do:
    name
    |> config_name()
    |> config_file_path()
    |> File.read!()
    |> Poison.decode!()


  # -------------------------------------------------------------------
  # Internal functions - Creating tables
  # -------------------------------------------------------------------

  defp setup_datasource(%{name: name} = data_source, {normal_data, encoded_data}) do
    IO.puts "Setting up #{name}"
    handler = handler_for_data_source(data_source)

    data_source
    |> handler.setup()
    |> handle_setup(table_definitions(&TableDefinitions.plain/1, data_source),
      handler, @normal_name_postfix, normal_data)
    |> handle_setup(table_definitions(&TableDefinitions.encoded/1, data_source),
      handler, @encoded_name_postfix, encoded_data)
    |> handler.terminate()

    IO.puts "#{name} done"
  end

  defp handle_setup(state, definitions, handler, table_postfix, data) do
    flattened_data = Data.flatten(data)
    collections = Data.to_collections(data)
    Enum.reduce(definitions, state, fn({name, %{columns: columns}}, state) ->
      state = handler.create_table("#{name}#{table_postfix}", columns, state)
      state = handler.insert_rows("#{name}#{table_postfix}", flattened_data[name], state)
      handler.insert_documents("#{name}#{table_postfix}", collections[name], state)
    end)
  end

  defp handler_for_data_source(%{driver: Cloak.DataSource.SAPHana}), do:
    Compliance.DataSource.SAPHana
  defp handler_for_data_source(%{driver: Cloak.DataSource.PostgreSQL}), do:
    Compliance.DataSource.PostgreSQL
  defp handler_for_data_source(%{driver: Cloak.DataSource.MySQL}), do:
    Compliance.DataSource.MySQL
  defp handler_for_data_source(%{driver: Cloak.DataSource.SQLServer}), do:
    Compliance.DataSource.SQLServer
  defp handler_for_data_source(%{driver: Cloak.DataSource.MongoDB}), do:
    Compliance.DataSource.MongoDB
  defp handler_for_data_source(%{driver: Cloak.DataSource.SQLServerTds}), do:
    Compliance.DataSource.SQLServerTds


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp config_name(other), do:
    other

  defp env(name), do: System.get_env(name) || ""

  defp config_file_path(name), do:
    Path.join([Application.app_dir(:cloak, "priv"), "config", "#{name}.json"])

  defp expand_and_add_table_definitions(data_source_scaffolds) do
    Enum.flat_map(data_source_scaffolds, fn(data_source_scaffold) ->
      normal_tables = table_definitions(&TableDefinitions.plain/1, data_source_scaffold)
      |> create_table_structure(@normal_name_postfix, data_source_scaffold)

      encoded_tables = table_definitions(&TableDefinitions.encoded/1, data_source_scaffold)
      |> create_table_structure(@encoded_name_postfix, data_source_scaffold)

      normal_data_source = data_source_scaffold
      |> Map.put(:tables, normal_tables)
      |> Map.put(:initial_tables, normal_tables)
      |> Map.put(:name, "#{data_source_scaffold.name}#{@normal_name_postfix}")
      |> Map.put(:marker, "normal")

      encoded_data_source = data_source_scaffold
      |> Map.put(:tables, encoded_tables)
      |> Map.put(:initial_tables, encoded_tables)
      |> Map.put(:name, "#{data_source_scaffold.name}#{@encoded_name_postfix}")
      |> Map.put(:marker, "encoded")

      [normal_data_source, encoded_data_source]
    end)
  end

  defp table_definitions(generator_fun, %{driver: Cloak.DataSource.MongoDB}), do:
    generator_fun.(true)
  defp table_definitions(generator_fun, _data_source), do:
    generator_fun.(false)

  defp create_table_structure(definitions, table_postfix, data_source_scaffold) do
    definitions
    |> Enum.map(fn({name, definition}) ->
      db_table_name = handler_for_data_source(data_source_scaffold).db_table_name(name)
      rawling = %{decoders: Map.get(definition, :decoders, %{})}
      |> add_uid_construct(name)
      |> Map.put(:db_name, "#{db_table_name}#{table_postfix}")
      {name, rawling}
    end)
    |> Enum.into(%{})
  end

  defp add_uid_construct(rawling, name) do
    case Map.get(TableDefinitions.uid_definitions(), name) do
      %{user_id: uid_column_name} -> Map.put(rawling, :user_id, uid_column_name)
      %{projection: projection} -> Map.put(rawling, :projection, projection)
    end
  end
end

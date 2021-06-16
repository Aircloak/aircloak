defmodule Compliance.DataSources do
  @moduledoc false

  alias Compliance.TableDefinitions

  @plain_name_postfix ""
  @encoded_name_postfix "_encoded"

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Creates configured data sources from a configuration file"
  @spec all_from_config_initialized(String.t()) :: [Cloak.DataSource.t()]
  def all_from_config_initialized(name),
    do:
      name
      |> all_from_config()
      |> complete_data_source_definitions()
      |> Stream.map(&adjust_data_source/1)
      |> Enum.map(&Cloak.DataSource.add_tables/1)

  @doc "Creates configured data sources from a configuration file without attempting to connect to the dataources"
  @spec all_from_config(String.t()) :: [Cloak.DataSource.t()]
  def all_from_config(name),
    do:
      read_config(name)["data_sources"]
      |> Cloak.DataSource.Utility.load_individual_data_source_configs()
      |> Enum.uniq_by(&{&1["parameters"], &1["driver"]})
      |> Cloak.DataSource.config_to_datasources()

  @doc "Creates tables for a normal and a encoded dataset and inserts data into them."
  @spec setup([DataSource.t()], Map.t(), non_neg_integer, pos_integer) :: :ok
  def setup(data_sources, data, num_users, concurrency) do
    chunk_size = 100
    # normalizes concurrency for smaller data sets to avoid opening needless connections
    concurrency = min(div(num_users + 1, chunk_size) + 1, concurrency)
    num_steps = Float.ceil(num_users / (chunk_size * concurrency))

    data_sources =
      Enum.uniq_by(
        data_sources,
        &{handler_for_data_source(&1), &1.parameters.hostname, &1.parameters.database}
      )

    data_sources
    |> Enum.map(&Task.async(fn -> setup_data_source(&1) end))
    |> Enum.each(&Task.await(&1, :timer.minutes(10)))

    insert_servers = Enum.map(data_sources, &start_insert_servers!(&1, concurrency))

    data
    |> Stream.chunk_every(chunk_size)
    |> Stream.map(&Enum.unzip/1)
    |> Stream.chunk_every(concurrency)
    |> Stream.map(fn chunks -> Enum.each(insert_servers, &insert_chunks(&1, chunks)) end)
    |> Stream.with_index()
    |> Stream.map(fn {_, chunk_index} ->
      IO.write("inserted users #{round(chunk_index / num_steps * 100)}%\r")
    end)
    |> Stream.run()

    Enum.each(insert_servers, &stop_insert_servers/1)
    IO.puts("\rinserted #{num_users} users                                   ")
  end

  @doc "Creates the database described by the given data source."
  @spec create(DataSource.t()) :: :ok
  def create(data_source) do
    handler = handler_for_data_source(data_source)
    conn = handler.setup(data_source)
    handler.terminate(conn)
  end

  @doc "Takes a data source template definition and expands it with table definitions"
  @spec complete_data_source_definitions([DataSource.t()], disable_analysis_operations: boolean) :: [DataSource.t()]
  def complete_data_source_definitions(data_source_definition_templates, opts \\ []) do
    Enum.flat_map(data_source_definition_templates, fn data_source_definition_template ->
      data_source_handler = handler_for_data_source(data_source_definition_template)

      plain_tables =
        TableDefinitions.plain()
        |> Enum.map(fn {name, definition} ->
          db_name = data_source_handler.db_table_name("#{Map.get(definition, :db_name, name)}#{@plain_name_postfix}")

          data_source_definition_template = %{
            db_name: db_name,
            content_type: Map.get(definition, :content_type, :private),
            keys: Map.get(definition, :keys, %{}),
            user_id: Map.get(definition, :user_id, nil),
            comments: Map.get(definition, :comments, %{})
          }

          {name, data_source_definition_template}
        end)
        |> Enum.into(%{})

      plain_data_source =
        data_source_definition_template
        |> Map.put(:tables, plain_tables)
        |> Map.put(:initial_tables, plain_tables)
        |> Map.put(:name, "#{data_source_definition_template.name}#{@plain_name_postfix}")

      if data_source_definition_template[:encoded] do
        encoded_tables =
          TableDefinitions.encoded(@encoded_name_postfix)
          |> Enum.map(fn {name, definition} ->
            data_source_definition_template = %{
              query: definition.query,
              content_type: Map.get(definition, :content_type, :private),
              keys: Map.get(definition, :keys, %{}),
              user_id: Map.get(definition, :user_id, nil),
              comments: Map.get(definition, :comments, %{})
            }

            {name, data_source_definition_template}
          end)
          |> Enum.into(%{})
          |> conditionally_disable_analysis_operations(Keyword.get(opts, :disable_analysis_operations, false))

        encoded_data_source =
          data_source_definition_template
          |> Map.put(:tables, encoded_tables)
          |> Map.put(:initial_tables, encoded_tables)
          |> Map.put(:name, "#{data_source_definition_template.name}#{@encoded_name_postfix}")

        [plain_data_source, encoded_data_source]
      else
        [plain_data_source]
      end
    end)
  end

  @doc "Returns a data source config as JSON"
  @spec read_config(String.t()) :: Map.t()
  def read_config(name),
    do:
      name
      |> config_name()
      |> config_file_path()
      |> File.read!()
      |> Jason.decode!()

  # -------------------------------------------------------------------
  # Data source setup
  # -------------------------------------------------------------------

  defp setup_data_source(data_source) do
    handler = handler_for_data_source(data_source)
    handler.setup(data_source)

    conn = handler.connect(data_source)

    create_tables(handler, conn, TableDefinitions.plain(), @plain_name_postfix)

    if data_source[:encoded],
      do: create_tables(handler, conn, TableDefinitions.encoded(@encoded_name_postfix), @encoded_name_postfix)

    conn
    |> handler.after_tables_created()
    |> handler.terminate()
  end

  defp create_tables(handler, conn, definitions, table_postfix) do
    Enum.each(definitions, fn {name, %{columns: columns}} ->
      handler.create_table("#{name}#{table_postfix}", columns, conn)
    end)
  end

  # -------------------------------------------------------------------
  # Insert servers
  # -------------------------------------------------------------------

  defp start_insert_servers!(data_source, concurrency) do
    Enum.map(1..concurrency, fn _ -> start_insert_server!(data_source) end)
  end

  defp start_insert_server!(data_source) do
    {:ok, insert_server} = Agent.start_link(fn -> nil end)

    Agent.cast(insert_server, fn nil ->
      handler = handler_for_data_source(data_source)
      conn = handler.connect(data_source)

      %{
        data_source: data_source,
        handler: handler,
        conn: conn,
        plain_definitions: TableDefinitions.plain(),
        encoded_definitions: TableDefinitions.encoded(@encoded_name_postfix)
      }
    end)

    insert_server
  end

  defp stop_insert_servers(insert_servers), do: Enum.each(insert_servers, &stop_insert_server/1)

  defp stop_insert_server(insert_server) do
    Agent.get(insert_server, & &1.handler.terminate(&1.conn), :timer.minutes(5))
  end

  defp insert_chunks(insert_servers, chunks) do
    chunks
    |> Stream.zip(insert_servers)
    |> Enum.each(fn {users, insert_server} -> insert_users(insert_server, users) end)
  end

  defp insert_users(insert_server, {plain_data, encoded_data}) do
    # The get followed by cast ensures simple buffering. We don't immediately wait for the outcome of the insertion,
    # which allows us to prepare the next chunk while the insertion is running. Then with this dummy sync lookup,
    # we make sure we don't send the next chunk until the current one has been processed.
    Agent.get(insert_server, fn _state -> :ok end, :timer.minutes(1))

    Agent.cast(insert_server, fn state ->
      insert_data(state, state.plain_definitions, plain_data, @plain_name_postfix)

      if state.data_source[:encoded],
        do: insert_data(state, state.encoded_definitions, encoded_data, @encoded_name_postfix)

      state
    end)
  end

  defp insert_data(state, definitions, data, table_postfix) do
    prepared_data = state.handler.prepare_data(data)

    definitions
    |> Enum.filter(fn {_, definition} -> definition[:db_name] == nil end)
    |> Enum.each(fn {name, _} ->
      state.handler.insert_rows("#{name}#{table_postfix}", prepared_data[name], state.conn)
    end)
  end

  defp handler_for_data_source(%{driver: Cloak.DataSource.PostgreSQL}), do: Compliance.DataSource.PostgreSQL
  defp handler_for_data_source(%{driver: Cloak.DataSource.Oracle}), do: Compliance.DataSource.Oracle
  defp handler_for_data_source(%{driver: Cloak.DataSource.ClouderaImpala}), do: Compliance.DataSource.ClouderaImpala

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp config_name(other), do: other

  defp config_file_path(name), do: Path.join([Application.app_dir(:cloak, "priv"), "config", "#{name}.json"])

  defp conditionally_disable_analysis_operations(tables, false), do: tables

  defp conditionally_disable_analysis_operations(tables, true) do
    feature_disabling_configuration = %{auto_isolating_column_classification: false, maintain_shadow_db: false}

    Enum.map(tables, fn {table_name, table_def} ->
      expanded_table_def = Map.merge(table_def, feature_disabling_configuration)
      {table_name, expanded_table_def}
    end)
  end

  defp adjust_data_source(data_source), do: handler_for_data_source(data_source).adjust_data_source(data_source)
end

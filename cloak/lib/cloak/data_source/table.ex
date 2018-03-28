defmodule Cloak.DataSource.Table do
  @moduledoc "Provides functionality for working with tables."

  alias Cloak.DataSource
  alias Cloak.Query.ExecutionError
  alias Cloak.Sql.{Compiler, Parser, Query.Lenses, CompilationError, Expression, Query}

  require Logger

  @type data_type :: :text | :integer | :real | :boolean | :datetime | :time | :date | :unknown
  @type column :: %{name: String.t(), type: data_type, visible?: boolean}

  @type t :: %{
          # table name as seen by the user
          :name => String.t(),
          # table name in the database
          :db_name => String.t() | nil,
          :user_id => String.t(),
          # the SQL query for a virtual table
          :query => Query.t() | nil,
          :columns => [column],
          :keys => [String.t()],
          optional(any) => any
        }

  @type projection :: %{table: String.t(), primary_key: String.t(), foreign_key: String.t()}

  @type option ::
          {:db_name, String.t()}
          | {:columns, [column]}
          | {:decoders, [Map.t()]}
          | {:projection, projection}
          | {:keys, [String.t()]}
          | {:query, Query.t()}
          | {atom, any}

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Creates the new table instance."
  @spec new(String.t(), String.t(), [option]) :: t
  def new(name, user_id_column_name, opts \\ []),
    do:
      Map.merge(
        %{
          name: name,
          user_id: user_id_column_name,
          db_name: nil,
          columns: [],
          decoders: [],
          projection: nil,
          keys: [],
          query: nil
        },
        Map.new(opts)
      )

  @doc "Creates the column entry in the table specification."
  @spec column(String.t(), data_type, visible?: boolean) :: column
  def column(name, type, optional_params \\ []),
    do: %{name: name, type: type, visible?: Keyword.get(optional_params, :visible?, true)}

  @doc "Given a data source and a connection to it, it will load all configured tables from the data set. "
  @spec load(DataSource.t(), DataSource.Driver.connection()) :: DataSource.t()
  def load(data_source, connection),
    do:
      data_source
      |> scan_tables(connection)
      |> resolve_projected_tables()
      |> translate_projections_and_decoders()
      |> scan_virtual_tables(connection)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp scan_virtual_tables(data_source, connection) do
    tables = Enum.map(data_source.tables, &parse_virtual_table/1)
    # in order to compile the raw SQL queries that generate virtual tables,
    # we need to create a virtual data source containing the real database tables referenced by those queries
    virtual_tables =
      tables
      |> Enum.flat_map(&get_virtual_table_db_names/1)
      |> Enum.uniq()
      |> Enum.reduce(%{}, fn table, acc ->
        if String.to_atom(table) in Map.keys(acc),
          do: acc,
          else: data_source |> load_tables(connection, {table, %{}}) |> Enum.into(acc)
      end)

    virtual_data_source = %{data_source | tables: virtual_tables}
    tables = Enum.map(tables, &compile_virtual_table(&1, virtual_data_source))
    %{data_source | tables: Enum.into(tables, %{})}
  end

  defp parse_virtual_table({name, %{query: statement} = table}) when statement != nil do
    case Parser.parse(statement) do
      {:ok, parsed_query} ->
        {name, %{table | query: parsed_query}}

      {:error, reason} ->
        DataSource.raise_error("Failed to parse the query for virtual table `#{name}`: `#{reason}`")
    end
  end

  defp parse_virtual_table(table), do: table

  defp get_virtual_table_db_names({_, %{query: parsed_query}}) when parsed_query != nil,
    do:
      Lenses.all_queries()
      |> Lenses.ast_tables()
      |> Lens.to_list(parsed_query)
      |> Enum.map(&ast_table_name/1)

  defp get_virtual_table_db_names(_), do: []

  defp ast_table_name({_, name}), do: name
  defp ast_table_name({table, :as, _alias}), do: ast_table_name(table)

  defp compile_virtual_table({name, %{query: parsed_query, user_id: user_id}}, data_source)
       when parsed_query != nil do
    compiled_query =
      try do
        parsed_query
        |> Map.put(:virtual_table?, true)
        |> Compiler.Specification.compile(data_source, nil, %{})
        |> drop_duplicate_columns()
        |> Map.put(:subquery?, true)
      rescue
        error in CompilationError ->
          reason = Exception.message(error)

          DataSource.raise_error("Failed to compile the query for virtual table `#{name}`: `#{reason}`")
      end

    Enum.each(compiled_query.column_titles, &verify_column_name(name, &1))

    columns =
      Enum.zip(compiled_query.column_titles, compiled_query.columns)
      |> Enum.map(fn {title, column} -> %{name: title, type: column.type, visible?: true} end)

    table = new(to_string(name), user_id, query: compiled_query, columns: columns)
    {name, table}
  end

  defp compile_virtual_table(table, _data_source), do: table

  defp verify_column_name(table, name) do
    if not Expression.valid_alias?(name),
      do:
        DataSource.raise_error(
          "Invalid column name `#{name}` in virtual table `#{table}`. " <>
            "Complex selected expressions have to be aliased with a valid column name."
        )
  end

  defp drop_duplicate_columns(query) do
    {column_titles, columns} =
      Enum.zip(query.column_titles, query.columns)
      |> Enum.uniq_by(fn {title, _column} -> title end)
      |> Enum.unzip()

    %Query{query | columns: columns, column_titles: column_titles}
  end

  defp scan_tables(%{errors: existing_errors} = data_source, connection) do
    {tables, errors} =
      Enum.reduce(data_source.tables, {[], []}, fn table, {tables, errors} ->
        try do
          {tables ++ load_tables(data_source, connection, table), errors}
        rescue
          error in ExecutionError ->
            {table_id, _} = table
            message = "Load error for table `#{table_id}`: #{Exception.message(error)}."
            Logger.error("Data source `#{data_source.name}`: #{message}")
            {tables, errors ++ [message]}
        end
      end)

    %{data_source | errors: existing_errors ++ errors, tables: Enum.into(tables, %{})}
  end

  defp load_tables(_data_source, _connection, {_, %{query: query}} = table) when query != nil, do: [table]

  defp load_tables(data_source, connection, {table_id, table}) do
    table_id = to_string(table_id)
    table = new(table_id, Map.get(table, :user_id), [db_name: table_id] ++ Map.to_list(table))

    data_source.driver.load_tables(connection, table)
    |> Enum.map(&parse_columns(data_source, &1))
    |> Enum.map(&{String.to_atom(&1.name), &1})
  end

  defp parse_columns(data_source, table) do
    table.columns
    |> Enum.reject(&supported?/1)
    |> validate_unsupported_columns(data_source, table)

    columns = for column <- table.columns, do: if(supported?(column), do: column, else: %{column | type: :unknown})

    table = %{table | columns: columns}
    verify_columns(data_source, table)
    table
  end

  defp verify_columns(data_source, table) do
    verify_user_id(data_source, table)
    if table.columns == [], do: DataSource.raise_error("no data columns found in table")
  end

  defp verify_user_id(_data_source, %{user_id: nil}), do: :ok

  defp verify_user_id(data_source, %{projection: projection} = table)
       when not is_nil(projection) do
    projected_uid_name = get_uid_name(data_source, projection.table, projection)

    case Enum.find(table.columns, &(&1.name == projected_uid_name)) do
      %{} ->
        DataSource.raise_error(
          "the projected uid-column named `#{projected_uid_name}` conflicts with an " <>
            "identically named column in the table. Rename the projected uid-column using the `user_id_alias` " <>
            "option in the projection section of your cloak configuration"
        )

      nil ->
        :ok
    end
  end

  defp verify_user_id(_data_source, table) do
    case Enum.find(table.columns, &(&1.name == table.user_id)) do
      %{} = column ->
        unless column.type in [:integer, :text, :real, :unknown],
          do: DataSource.raise_error("unsupported user id type: #{column.type}")

      nil ->
        columns_string =
          table.columns
          |> Enum.map(&"`#{&1.name}`")
          |> Enum.join(", ")

        DataSource.raise_error(
          "the user id column `#{table.user_id}` for table `#{table.name}` does not exist. " <>
            "Available columns are: #{columns_string}."
        )
    end
  end

  defp get_uid_name(data_source, table_name, nil) do
    table = table_from_datasource(data_source, table_name)
    table.user_id
  end

  defp get_uid_name(_data_source, _table, %{user_id_alias: alias}), do: alias

  defp get_uid_name(data_source, _table, %{table: table_name}) do
    table = table_from_datasource(data_source, table_name)
    get_uid_name(data_source, table_name, table[:projection])
  end

  defp table_from_datasource(data_source, table_name), do: DataSource.table(data_source, String.to_atom(table_name))

  defp supported?(%{type: {:unsupported, _db_type}}), do: false
  defp supported?(_column), do: true

  defp validate_unsupported_columns([], _data_source, _table), do: :ok

  defp validate_unsupported_columns(unsupported, data_source, table) do
    columns_string =
      unsupported
      |> Enum.map(fn column -> "`#{column.name}`::#{inspect(column.type)}" end)
      |> Enum.join(", ")

    Logger.warn(
      "The following columns from table `#{table[:db_name]}` in data source `#{data_source.name}` " <>
        "have unsupported types:\n" <> columns_string
    )

    :ok
  end

  defp resolve_projected_tables(data_source) do
    # remove the set user_id for projected tables
    tables =
      data_source.tables
      |> Enum.map(fn
        {id, %{projection: %{}} = table} -> {id, %{table | user_id: nil}}
        {id, table} -> {id, table}
      end)
      |> Enum.into(%{})

    data_source = %{data_source | tables: tables}

    tables
    |> Map.keys()
    |> Enum.reduce(data_source, &resolve_projected_table(Map.fetch!(&2.tables, &1), &2))
  end

  defp resolve_projected_table(%{query: query}, data_source) when query != nil, do: data_source
  defp resolve_projected_table(%{projection: nil}, data_source), do: data_source

  defp resolve_projected_table(%{user_id: uid, columns: [%{name: uid} | _]}, data_source),
    # uid column is resolved
    do: data_source

  defp resolve_projected_table(table, data_source) do
    case validate_projection(data_source.tables, table) do
      {:ok, referenced_table} ->
        # recursively resolve dependent table (this allows the dependent table to be projected as well)
        data_source = resolve_projected_table(referenced_table, data_source)
        # refetch the table, since it's maybe updated
        referenced_table = Map.fetch!(data_source.tables, String.to_atom(referenced_table.name))

        uid_column_name = referenced_table.user_id

        uid_column =
          referenced_table.columns
          |> Enum.find(&(&1.name == uid_column_name))
          |> set_display_name(table)

        table =
          table
          |> Map.put(:user_id, uid_column.name)
          |> Map.put(:columns, [uid_column | table.columns])

        tables = Map.put(data_source.tables, String.to_atom(table.name), table)
        %{data_source | tables: tables}

      {:error, reason} ->
        message = "Projection error in table `#{table.name}`: #{reason}."
        Logger.error("Data source `#{data_source.name}`: #{message}")
        tables = Map.delete(data_source.tables, String.to_atom(table.name))
        %{data_source | tables: tables, errors: data_source.errors ++ [message]}
    end
  end

  defp set_display_name(column, %{projection: %{user_id_alias: alias}}), do: %{column | name: alias}

  defp set_display_name(column, _), do: column

  defp validate_projection(tables_map, table) do
    with :ok <- validate_foreign_key(table),
         {:ok, referenced_table} <- validate_referenced_table(tables_map, table),
         :ok <- validate_primary_key(table, referenced_table),
         do: {:ok, referenced_table}
  end

  defp validate_referenced_table(tables_map, table) do
    case Map.fetch(tables_map, String.to_atom(table.projection.table)) do
      :error -> {:error, "referenced table `#{table.projection.table}` not found."}
      {:ok, referenced_table} -> {:ok, referenced_table}
    end
  end

  defp validate_foreign_key(table) do
    case Enum.find(table.columns, &(&1.name == table.projection.foreign_key)) do
      nil -> {:error, "foreign key column `#{table.projection.foreign_key}` doesn't exist"}
      _ -> :ok
    end
  end

  defp validate_primary_key(table, referenced_table) do
    foreign_key_type = Enum.find(table.columns, &(&1.name == table.projection.foreign_key)).type

    case Enum.find(referenced_table.columns, &(&1.name == table.projection.primary_key)) do
      nil ->
        {
          :error,
          "primary key column `#{table.projection.primary_key}` not found in table `#{referenced_table.db_name}`"
        }

      %{type: primary_key_type} when primary_key_type != foreign_key_type ->
        {
          :error,
          "foreign key type is `#{foreign_key_type}` while primary key type is `#{primary_key_type}`"
        }

      %{type: ^foreign_key_type} ->
        :ok
    end
  end

  defp translate_projections_and_decoders(%{tables: tables} = data_source),
    do: %{data_source | tables: Enum.map(tables, &translate_projection_and_decoders(&1, tables))}

  defp translate_projection_and_decoders(
         {id, %{projection: projection, decoders: decoders} = table},
         tables
       )
       when is_map(projection) or (is_list(decoders) and length(decoders) > 0) do
    {user_id, alias, source} = translate_projection({id, table}, tables)

    columns =
      decoders
      |> translate_decoders(id)
      |> Enum.reduce("\"#{id}\".*\n", fn {name, expression}, acc ->
        "#{expression} AS \"#{name}\",\n #{acc}"
      end)

    query = "SELECT #{user_id} AS #{alias},\n #{columns} FROM #{source}"
    {id, table |> Map.drop([:projection, :decoders]) |> Map.put(:query, query)}
  end

  defp translate_projection_and_decoders(table, _), do: table

  defp translate_decoders(decoders, table_name) do
    Enum.reduce(decoders, %{}, fn decoder, acc ->
      Enum.reduce(decoder.columns, acc, fn column, acc ->
        current_expression = Map.get(acc, column, "\"#{table_name}\".\"#{column}\"")
        Map.put(acc, column, translate_decoder(decoder, current_expression))
      end)
    end)
  end

  defp translate_decoder(%{method: "aes_cbc_128", key: key}, column),
    do: "dec_aes_cbc128(#{column}, '#{String.replace(key, "'", "''")}')"

  defp translate_decoder(%{method: "text_to_integer"}, column), do: "CAST(#{column} AS integer)"
  defp translate_decoder(%{method: "real_to_integer"}, column), do: "CAST(#{column} AS integer)"
  defp translate_decoder(%{method: "text_to_real"}, column), do: "CAST(#{column} AS real)"
  defp translate_decoder(%{method: "text_to_datetime"}, column), do: "CAST(#{column} AS datetime)"
  defp translate_decoder(%{method: "text_to_date"}, column), do: "CAST(#{column} AS date)"
  defp translate_decoder(%{method: "text_to_boolean"}, column), do: "CAST(#{column} AS boolean)"
  defp translate_decoder(%{method: "real_to_boolean"}, column), do: "CAST(#{column} AS boolean)"
  defp translate_decoder(%{method: "base64"}, column), do: "dec_b64(#{column})"

  defp translate_decoder(%{method: method}, _column),
    do: DataSource.raise_error("Invalid decoding method specified: #{method}")

  defp quote_db_name("\"" <> _ = name), do: name
  defp quote_db_name(name), do: ~s/"#{name}"/

  defp translate_projection({id, %{projection: nil, user_id: user_id, db_name: db_name}}, _),
    do: {~s/"#{id}"."#{user_id}"/, ~s/"#{user_id}"/, ~s/#{quote_db_name(db_name)} AS "#{id}"/}

  defp translate_projection({id, %{projection: projection, db_name: db_name}}, tables) do
    projection_id = String.to_atom(projection.table)
    {user_id, alias, from} = translate_projection({projection_id, tables[projection_id]}, tables)

    {user_id, alias, from} =
      if tables[projection_id].projection != nil do
        # for mongodb data sources, it is faster to use a subquery here as the driver doesn't support complex joins
        from =
          ~s/(SELECT #{user_id} AS #{alias}, "#{projection_id}"."#{projection.primary_key}" / <>
            ~s/FROM #{from}) AS "#{projection_id}"/

        {~s/"#{projection_id}".#{alias}/, alias, from}
      else
        {user_id, alias, from}
      end

    from =
      ~s/#{from} JOIN #{quote_db_name(db_name)} AS "#{id}" ON "#{id}"."#{projection.foreign_key}" = / <>
        ~s/"#{projection_id}"."#{projection.primary_key}"/

    alias = if projection[:user_id_alias] != nil, do: ~s/"#{projection.user_id_alias}"/, else: alias

    {user_id, alias, from}
  end
end

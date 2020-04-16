defmodule Cloak.DataSource.Table do
  @moduledoc "Provides functionality for working with tables."

  alias Cloak.DataSource
  alias Cloak.Query.ExecutionError
  alias Cloak.Sql.{Compiler, Parser, Query.Lenses, CompilationError, Expression, Query}

  require Logger

  @type data_type :: :text | :integer | :real | :boolean | :datetime | :time | :date | :interval | :unknown
  @type column :: %{name: String.t(), type: data_type, visible?: boolean}
  @type join_link :: {String.t(), atom, String.t()}

  @type t :: %{
          # table name as seen by the user
          :name => String.t(),
          # table name in the database
          :db_name => String.t() | nil,
          :user_id => String.t(),
          # the SQL query for a virtual table
          :query => Query.t() | nil,
          :columns => [column],
          :exclude_columns => [String.t()],
          :unselectable_columns => [String.t()],
          :keys => Map.t(),
          :content_type => :private | :public,
          :auto_isolating_column_classification => boolean,
          :isolating_columns => Map.t(),
          :maintain_shadow_db => boolean,
          :status => :created | :creating | :create_error,
          :user_id_join_chain => [join_link] | nil,
          :type => type,
          optional(any) => any
        }

  @type type :: :regular | :virtual | :analyst | :subquery

  @type option ::
          {:db_name, String.t()}
          | {:columns, [column]}
          | {:exclude_columns, [String.t()]}
          | {:unselectable_columns, [String.t()]}
          | {:keys, Map.t()}
          | {:query, Query.t()}
          | {:content_type, :private | :public}
          | {:type, type}
          | {atom, any}

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Creates the new table instance."
  @spec new(String.t(), String.t() | nil, [option] | Map.t()) :: t
  def new(name, user_id_column_name, opts \\ []) do
    table =
      %{
        name: name,
        user_id: user_id_column_name,
        db_name: nil,
        columns: [],
        keys: %{},
        content_type: if(user_id_column_name == nil, do: :public, else: :private),
        query: nil,
        auto_isolating_column_classification: true,
        isolating_columns: %{},
        maintain_shadow_db: true,
        status: :created,
        user_id_join_chain: if(user_id_column_name == nil, do: nil, else: []),
        type: :regular
      }
      |> Map.merge(Map.new(opts))
      |> remove_excluded_columns()

    keys = if(user_id_column_name == nil, do: table.keys, else: Map.put(table.keys, user_id_column_name, :user_id))
    %{table | keys: keys}
  end

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
      |> scan_virtual_tables(connection)
      |> resolve_tables_keys()
      |> resolve_user_id_join_chains()

  @doc "Maps configured tables into the proper table structure."
  @spec map_tables(Map.t()) :: Map.t()
  def map_tables(data_source) do
    %{data_source | tables: data_source.tables |> Enum.map(&map_table/1) |> Enum.into(%{})}
  rescue
    error in ExecutionError ->
      reason = Exception.message(error)
      # credo:disable-for-next-line Credo.Check.Warning.RaiseInsideRescue
      raise ExecutionError, message: "Error in configured tables for data source `#{data_source.name}`: #{reason}"
  end

  @doc """
  Returns the value for a type with the highest chance of being invalid.

  This is used by the join timing protection mechanism to create a fake user row that won't match anything else in the data set.
  We can't use NULL values for the row, as the optimizer can detect it won't match the filtering conditions and it will drop it prematurely.
  """
  @spec invalid_value(data_type) :: any
  def invalid_value(:integer), do: -2_147_483_648
  def invalid_value(:real), do: -3.4e+38
  def invalid_value(:text), do: ""
  def invalid_value(:date), do: ~D[3000-01-01]
  def invalid_value(:datetime), do: ~N[3000-01-01 00:00:00]
  def invalid_value(:time), do: ~T[00:00:00]
  def invalid_value(:boolean), do: false
  def invalid_value(:interval), do: Timex.Duration.zero()
  def invalid_value(_), do: nil

  @doc "Returns true if the column with the given name is a key in this table, false otherwise."
  @spec key?(t, String.t()) :: boolean
  def key?(table, column_name), do: Map.has_key?(table.keys, column_name)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp scan_virtual_tables(data_source, connection) do
    tables = Enum.map(data_source.tables, &parse_virtual_table/1)
    # in order to compile the raw SQL queries that generate virtual tables,
    # we need to create a virtual data source containing the real database tables referenced by those queries
    virtual_tables =
      tables
      |> Enum.flat_map(&get_db_tables_for_virtual_query/1)
      |> Enum.uniq_by(fn {id, _} -> id end)
      |> Enum.reduce(%{}, fn {id, table}, acc ->
        if id in Map.keys(acc) do
          acc
        else
          case data_source.tables[id] do
            %{columns: [_ | _]} = loaded_table -> Map.put(acc, id, Map.put(loaded_table, :query, nil))
            _ -> data_source |> load_tables(connection, {id, table}) |> Enum.into(acc)
          end
        end
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
        raise ExecutionError, message: "Failed to parse the query for virtual table `#{name}`: `#{reason}`"
    end
  end

  defp parse_virtual_table(table), do: table

  defp get_db_tables_for_virtual_query({_, %{query: parsed_query} = table}) when parsed_query != nil,
    do:
      Lenses.all_queries()
      |> Lenses.ast_tables()
      |> Lens.to_list(parsed_query)
      |> Enum.map(&{&1 |> ast_table_name() |> String.to_atom(), Map.take(table, [:sample_rate])})

  defp get_db_tables_for_virtual_query(_), do: []

  defp ast_table_name({_, name}), do: name
  defp ast_table_name({table, :as, _alias}), do: ast_table_name(table)

  defp compile_virtual_table({name, config = %{query: parsed_query}}, data_source)
       when parsed_query != nil do
    compiled_query =
      try do
        parsed_query
        |> Map.put(:subquery?, true)
        |> Compiler.compile_standard!(nil, data_source)
        |> drop_duplicate_columns()
        |> drop_constant_columns()
      rescue
        error in CompilationError ->
          reason = Exception.message(error)

          # credo:disable-for-next-line Credo.Check.Warning.RaiseInsideRescue
          raise ExecutionError, message: "Failed to compile the query for virtual table `#{name}`: `#{reason}`"
      end

    Enum.each(compiled_query.column_titles, &verify_column_name(name, &1))

    columns =
      Enum.zip(compiled_query.column_titles, compiled_query.columns)
      |> Enum.map(fn {title, column} -> %{name: title, type: column.type, visible?: true} end)

    table = new(to_string(name), config[:user_id], Map.merge(config, %{query: compiled_query, columns: columns}))
    verify_columns(data_source, table)
    {name, table}
  end

  defp compile_virtual_table(table, _data_source), do: table

  defp verify_column_name(table, name) do
    if not Expression.valid_alias?(name) do
      raise ExecutionError,
        message:
          "Invalid column name `#{name}` in virtual table `#{table}`. " <>
            "Complex selected expressions have to be aliased with a valid column name."
    end
  end

  defp drop_duplicate_columns(query) do
    {column_titles, columns} =
      Enum.zip(query.column_titles, query.columns)
      |> Enum.uniq_by(fn {title, _column} -> title end)
      |> Enum.unzip()

    %Query{query | columns: columns, column_titles: column_titles}
  end

  defp drop_constant_columns(query) do
    {column_titles, columns} =
      Enum.zip(query.column_titles, query.columns)
      |> Enum.reject(fn {_title, column} -> Expression.constant?(column) end)
      |> Enum.unzip()

    %Query{query | columns: columns, column_titles: column_titles}
  end

  defp scan_tables(%{errors: existing_errors} = data_source, connection) do
    {tables, errors} =
      Enum.reduce(data_source.tables, {[], []}, fn {table_id, _table_definition} = table, {tables, errors} ->
        try do
          # The `public.` prefix is forbidden to prevent ambiguity when handling `select ... from public.some_table`.
          if to_string(table_id) =~ ~r/^public\./,
            do: raise(ExecutionError, message: "table name can't start with `public.`")

          {tables ++ load_tables(data_source, connection, table), errors}
        rescue
          error in ExecutionError ->
            message =
              "Load error for table `#{table_id}`: #{Exception.message(error)}. " <>
                "Please check that the specified database table exists, it has the correct spelling, " <>
                "and that the specified database user has sufficient rights to access it."

            Logger.error("Data source `#{data_source.name}`: #{message}")
            {tables, errors ++ [message]}
        end
      end)

    %{data_source | errors: existing_errors ++ errors, tables: Enum.into(tables, %{})}
  end

  defp load_tables(_data_source, _connection, {table_id, %{query: query} = table}) when query != nil,
    do: [{table_id, Map.put(table, :type, :virtual)}]

  defp load_tables(data_source, connection, {table_id, table}) do
    table_id = to_string(table_id)
    table = new(table_id, Map.get(table, :user_id), [type: :regular, db_name: table_id] ++ Map.to_list(table))

    data_source.driver.load_tables(connection, table)
    |> Enum.map(&parse_columns(data_source, &1))
    |> Enum.map(&{String.to_atom(&1.name), &1})
    |> Enum.map(&resolve_table_keys/1)
  end

  defp parse_columns(data_source, table) do
    current_columns = Enum.reject(table.columns, &exclude_column?(table, &1))

    current_columns
    |> Enum.reject(&supported?/1)
    |> validate_unsupported_columns(data_source, table)

    columns =
      Enum.map(current_columns, fn column ->
        if(supported?(column), do: column, else: %{column | type: :unknown})
      end)

    table = %{table | columns: columns}
    verify_columns(data_source, table)
    table
  end

  defp verify_columns(data_source, table) do
    verify_user_id(data_source, table)
    if table.columns == [], do: raise(ExecutionError, message: "no data columns found in table")
  end

  defp verify_user_id(_data_source, %{user_id: nil}), do: :ok

  defp verify_user_id(_data_source, table) do
    case Enum.find(table.columns, &(&1.name == table.user_id)) do
      %{} = column ->
        unless column.type in [:integer, :text, :real, :unknown],
          do: raise(ExecutionError, message: "unsupported user id type: #{column.type}")

      nil ->
        columns_string =
          table.columns
          |> Enum.map(&"`#{&1.name}`")
          |> Enum.join(", ")

        raise ExecutionError,
          message:
            "the user id column `#{table.user_id}` for table `#{table.name}` does not exist. " <>
              "Available columns are: #{columns_string}."
    end
  end

  defp supported?(%{type: {:unsupported, _db_type}}), do: false
  defp supported?(_column), do: true

  defp exclude_column?(table, column), do: column.name in Map.get(table, :exclude_columns, [])

  defp remove_excluded_columns(%{columns: columns, exclude_columns: exclude_columns} = table) do
    %{table | columns: Enum.reject(columns, &(&1.name in exclude_columns))}
  end

  defp remove_excluded_columns(table), do: table

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

  defp resolve_tables_keys(data_source) do
    %{data_source | tables: data_source.tables |> Enum.map(&resolve_table_keys/1) |> Enum.into(%{})}
  end

  defp resolve_table_keys({name, %{keys: keys} = table}) do
    column_names = Enum.map(table.columns, & &1.name)

    keys
    |> Enum.map(fn {name, _tag} -> name end)
    |> Enum.reject(&(&1 in column_names))
    |> case do
      [] ->
        :ok

      [invalid_name | _] ->
        raise(ExecutionError, message: "Invalid key name: column `#{invalid_name}` doesn't exist in table `#{name}`")
    end

    keys = if table.user_id != nil, do: Map.put(keys, table.user_id, :user_id), else: keys

    user_id =
      Enum.filter(keys, fn {_name, type} -> type == :user_id end)
      |> case do
        [{user_id, :user_id} | _] -> user_id
        [] -> nil
      end

    {name, %{table | keys: keys, user_id: user_id}}
  end

  defp map_table({name, table}) do
    {name, table}
    |> map_isolators()
    |> map_keys()
    |> map_content_type()
  end

  def map_isolators({name, %{isolating_columns: _} = table}),
    do: {name, update_in(table, [Lens.key(:isolating_columns) |> Lens.map_keys()], &to_string/1)}

  def map_isolators({name, table}), do: {name, table}

  defp map_content_type({name, %{user_id: nil}}) do
    raise ExecutionError,
      message:
        "Table `#{name}` has the `user_id` field set to `null`. This is not supported. " <>
          "Set the `content_type` field to `non-personal` instead and remove the `user_id` entry."
  end

  defp map_content_type({name, %{user_id: _} = table}) do
    if table[:content_type] != nil do
      raise ExecutionError, message: "The `user_id` and `content_type` fields are both set for table `#{name}`."
    end

    {name, table}
  end

  defp map_content_type({name, table}) do
    if table[:content_type] == "non-personal" do
      if table.keys |> Map.values() |> Enum.any?(&(&1 == :user_id)) do
        raise ExecutionError, message: "Table `#{name}` with content type `non-personal` has a `user_id` key set."
      end

      {name, Map.put(table, :content_type, :public)}
    else
      true = table[:content_type] in [nil, "personal"]
      {name, Map.put(table, :content_type, :private)}
    end
  end

  defp map_keys({name, %{keys: keys} = table}) do
    keys = Enum.map(keys, &map_key(&1, name))
    key_columns = Keyword.keys(keys)

    case key_columns -- Enum.uniq(key_columns) do
      [] ->
        :ok

      [column | _] ->
        key_types =
          keys
          |> Enum.filter(fn {name, _type} -> name == column end)
          |> Enum.map(fn {_name, type} -> "`#{type}`" end)
          |> Aircloak.OxfordComma.join()

        raise ExecutionError, message: "Column `#{column}` in table `#{name}` has multiple key types set: #{key_types}"
    end

    {name, %{table | keys: Enum.into(keys, %{})}}
  end

  defp map_keys({name, table}), do: {name, table}

  defp map_key(%{} = key, table_name) do
    if map_size(key) != 1 do
      raise ExecutionError,
        message: ~s(Invalid key entry for table `#{table_name}`. A key has the format `{"key_type": "column_name"}`.)
    end

    [type] = Map.keys(key)
    [column] = Map.values(key)
    {column, type}
  end

  defp resolve_user_id_join_chains(data_source) do
    max_chain_length = Enum.count(data_source.tables)

    tables =
      data_source.tables
      |> Enum.map(fn {name, table} ->
        user_id_join_chain = resolve_user_id_join_chain(data_source, name, max_chain_length)

        if table.content_type == :private and user_id_join_chain == nil do
          raise ExecutionError,
            message:
              "Table `#{name}` can not be queried: content type is set to `personal`, " <>
                "but table has no possible join path to a key of type `user_id`."
        else
          {name, %{table | user_id_join_chain: user_id_join_chain}}
        end
      end)
      |> Enum.into(%{})

    %{data_source | tables: tables}
  end

  defp resolve_join_link({_table1_name, table1}, {table2_name, table2}) do
    table1.keys
    |> Stream.map(fn {column1, type1} ->
      Enum.find_value(table2.keys, fn {column2, type2} ->
        if type1 == type2,
          do: {column1, table2_name, column2},
          else: nil
      end)
    end)
    |> Stream.reject(&(&1 == nil))
    |> Enum.at(0)
  end

  defp resolve_user_id_join_chain(data_source, table_name, max_length, visited_tables \\ []) do
    table = data_source.tables[table_name]

    cond do
      max_length == 0 ->
        nil

      table.user_id != nil ->
        []

      true ->
        visited_tables = [table_name | visited_tables]

        data_source.tables
        |> Stream.reject(fn {name, _} -> name in visited_tables end)
        |> Stream.map(&resolve_join_link({table_name, table}, &1))
        |> Stream.reject(&(&1 == nil))
        |> Enum.reduce(nil, fn {_, link_name, _} = link, current_chain ->
          max_length = if current_chain == nil, do: max_length, else: length(current_chain)

          data_source
          |> resolve_user_id_join_chain(link_name, max_length - 1, visited_tables)
          |> case do
            nil -> current_chain
            smaller_chain -> [link | smaller_chain]
          end
        end)
    end
  end
end

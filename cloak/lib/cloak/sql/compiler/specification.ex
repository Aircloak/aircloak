defmodule Cloak.Sql.Compiler.Specification do
  @moduledoc "Turns a parsed SQL AST into a `Cloak.Sql.Query` specification describing the user query."

  alias Cloak.DataSource
  alias Cloak.Sql.{Condition, CompilationError, Expression, Function, Query}
  alias Cloak.Sql.Compiler.{Helpers, Validation}
  alias Cloak.Sql.Query.Lenses

  @dummy_location {1, 0}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Compiles the parsed query."
  @spec compile(Parser.parsed_query, DataSource.t, [Query.parameter] | nil, Query.view_map) :: Query.t
  def compile(parsed_query, data_source, parameters, views), do:
    %Query{
      data_source: data_source,
      parameters: parameters,
      views: views
    }
    |> Map.merge(parsed_query)
    |> compile_query()


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp compile_query(%Query{command: :show, show: :tables} = query), do:
    %Query{query |
      column_titles: ["name"],
      columns: [%Expression{table: :unknown, constant?: true, name: "name", type: :text}]
    }
  defp compile_query(%Query{command: :show, show: :columns} = query), do:
    %Query{compile_from(query) |
      column_titles: ["name", "type"],
      columns: Enum.map(["name", "type"], &%Expression{table: :unknown, constant?: true, name: &1, type: :text})
    }
  defp compile_query(%Query{command: :select} = query), do:
    query
    |> compile_from()
    |> compile_parameter_types()
    |> expand_star_select()
    |> compile_aliases()
    |> compile_columns()
    |> compile_references()
    |> cast_where_clauses()


  # -------------------------------------------------------------------
  # From
  # -------------------------------------------------------------------

  defp compile_from(query), do:
    query
    |> compile_views()
    |> normalize_from()
    |> compile_projected_tables()
    |> compile_subqueries()
    |> compile_selected_tables()

  defp normalize_from(%Query{} = query), do:
    query
    |> collect_table_aliases()
    |> update_in([Lenses.ast_tables()], &normalize_from(&1, query.data_source))

  defp collect_table_aliases(query) do
    aliases =
      query
      |> get_in([Lenses.ast_tables()])
      |> Enum.filter(&match?({_table_identifier, :as, _alias}, &1))
      |> Enum.map(fn({table_identifier, :as, alias}) -> {alias, find_table!(query.data_source, table_identifier)} end)

    verify_duplicate_aliases(aliases)

    %Query{query | table_aliases: Map.new(aliases)}
  end

  defp verify_duplicate_aliases(aliases) do
    aliases
    |> Enum.map(fn({alias, _}) -> alias end)
    |> Enum.group_by(&(&1))
    |> Enum.reject(&match?({_alias, [_]}, &1))
    |> Enum.map(fn({alias, _}) -> alias end)
    |> case do
      [duplicate_alias | _] ->
        raise CompilationError, message: "Table alias `#{duplicate_alias}` used more than once."
      [] -> :ok
    end
  end

  defp normalize_from({_table_identifier, :as, alias}, _data_source), do:
    alias
  defp normalize_from(table_identifier, data_source), do:
    find_table!(data_source, table_identifier).name

  defp find_table!(data_source, table_identifier = {_, table_name}) do
    case data_source |> DataSource.tables() |> find_table(table_identifier) do
      nil -> raise CompilationError, message: "Table `#{table_name}` doesn't exist."
      table -> table
    end
  end

  defp find_table(tables, {:quoted, name}), do: Enum.find(tables, &name == &1.name)
  defp find_table(tables, {:unquoted, name}), do: Enum.find(tables, &insensitive_equal?(name, &1.name))

  defp insensitive_equal?(s1, s2), do: String.downcase(s1) == String.downcase(s2)


  # -------------------------------------------------------------------
  # Views
  # -------------------------------------------------------------------

  defp compile_views(query) do
    compiled = do_compile_views(query.from, query)
    %Query{query | from: compiled}
  end

  defp do_compile_views({:join, join}, query) do
    {:join, %{join |
      lhs: do_compile_views(join.lhs, query),
      rhs: do_compile_views(join.rhs, query)
    }}
  end
  defp do_compile_views({_, name} = table_or_view, query) when is_binary(name) do
    case Map.fetch(query.views, name) do
      {:ok, view_sql} -> view_to_subquery(name, view_sql, query)
      :error -> table_or_view
    end
  end
  defp do_compile_views({table_or_view, :as, alias}, query) do
    case do_compile_views(table_or_view, query) do
      {:subquery, subquery} -> {:subquery, %{subquery | alias: alias}}
      other -> {other, :as, alias}
    end
  end
  defp do_compile_views({:subquery, subquery}, _query), do:
    {:subquery, subquery}

  defp view_to_subquery(view_name, view_sql, query) do
    if Enum.any?(
      query.data_source.tables,
      fn({_id, table}) -> insensitive_equal?(table.name, view_name) end
    ) do
      raise CompilationError,
        message: "There is both a table, and a view named `#{view_name}`. Rename the view to resolve the conflict."
    end

    case Cloak.Sql.Parser.parse(view_sql) do
      {:ok, parsed_view} -> {:subquery, %{ast: Map.put(parsed_view, :view?, true), alias: view_name}}
      {:error, error} -> raise CompilationError, message: "Error in the view `#{view_name}`: #{error}"
    end
  end


  # -------------------------------------------------------------------
  # Projected tables
  # -------------------------------------------------------------------

  defp compile_projected_tables(%Query{projected?: true} = query), do: query
  defp compile_projected_tables(query), do:
    Lens.map(Lenses.leaf_tables(), query, &compile_projected_table(&1, query))

  defp compile_projected_table(table_name, query) do
    case table_from_name_or_alias!(query, table_name) do
      %{projection: nil} -> table_name
      projected_table -> projected_table_ast(projected_table, table_name, :all, query)
    end
  end

  defp projected_table_ast(%{projection: nil}, table_alias, _column_to_select, _query), do:
    {:quoted, table_alias}
  defp projected_table_ast(table, table_alias, columns_to_select, query) do
    # note that we're fetching the table from the data source, because projection name is not an alias
    joined_table = DataSource.table(query.data_source, table.projection.table)
    {:subquery, %{
      alias: table_alias,
      table_name: table.name,
      ast: %{
        command: :select,
        projected?: true,
        columns:
          [uid_column_ast(joined_table.name, joined_table.user_id, table) |
            table.columns
            |> Enum.map(&(&1.name))
            |> Enum.reject(&(&1 == table.user_id))
            |> Enum.filter(&(columns_to_select == :all || Enum.member?(columns_to_select, &1)))
            |> Enum.map(&column_ast(table.name, &1))
          ],
        from:
          {:join, %{
            type: :inner_join,
            lhs: {:quoted, table.name},
            rhs: projected_table_ast(joined_table, joined_table.name, [table.projection.primary_key], query),
            conditions: {:comparison,
              column_ast(table.name, table.projection.foreign_key),
              :=,
              column_ast(joined_table.name, table.projection.primary_key)
            }
          }}
      }
    }}
  end

  defp uid_column_ast(table_name, column_name, %{projection: projection}), do:
    {column_ast(table_name, column_name), :as, projection[:user_id_alias] || column_name}

  defp column_ast(table_name, column_name), do:
    {:identifier, {:quoted, table_name}, {:quoted, column_name}, @dummy_location}

  # -------------------------------------------------------------------
  # Subqueries
  # -------------------------------------------------------------------

  defp compile_subqueries(query), do:
    Lens.map(Lenses.direct_subqueries(), query,  &%{&1 | ast: compile_subquery(&1.ast, &1.alias, query)})

  defp compile_subquery(parsed_subquery, alias, parent_query), do:
    parsed_subquery
    |> Map.put(:subquery?, true)
    |> compile(parent_query.data_source, parent_query.parameters, parent_query.views)
    |> ensure_uid_selected(alias)


  # -------------------------------------------------------------------
  # Selected tables
  # -------------------------------------------------------------------

  defp compile_selected_tables(query), do:
    %Query{query | selected_tables: selected_tables(query.from, query)}

  defp selected_tables({:join, join}, query), do:
    selected_tables(join.lhs, query) ++ selected_tables(join.rhs, query)
  defp selected_tables({:subquery, subquery}, _query) do
    # In a subquery we should have the `user_id` already in the list of selected columns.
    user_id_index = Enum.find_index(subquery.ast.columns, &(&1.user_id?))
    user_id_name = Enum.at(subquery.ast.column_titles, user_id_index)
    columns =
        Enum.zip(subquery.ast.column_titles, subquery.ast.columns)
        |> Enum.map(fn({alias, column}) ->
          DataSource.Table.column(alias, Function.type(column), visible?: not column.synthetic?) end)
        |> Enum.uniq()
    keys =
      Enum.zip(subquery.ast.column_titles, subquery.ast.columns)
      |> Enum.filter(fn({_, column}) -> column.key? end)
      |> Enum.map(fn({title, _}) -> title end)

    [DataSource.Table.new(subquery.alias, user_id_name, columns: columns, keys: keys)]
  end
  defp selected_tables(table_name, query) when is_binary(table_name), do:
    [%{table_from_name_or_alias!(query, table_name) | name: table_name}]

  defp table_from_name_or_alias!(query, table_name) do
    table = Map.get(query.table_aliases, table_name, DataSource.table(query.data_source, table_name))
    true = (table != nil)
    table
  end

  # -------------------------------------------------------------------
  # Parameter types
  # -------------------------------------------------------------------

  defp compile_parameter_types(query), do:
    query
    |> do_compile_parameter_types()
    |> check_missing_parameter_types()

  defp do_compile_parameter_types(%Query{parameters: parameters} = query) when is_list(parameters), do:
    # Parameters are bound
    parameters
    |> Enum.with_index()
    |> Enum.reduce(query, fn({param, index}, query) -> Query.set_parameter_type(query, index + 1, param.type) end)
  defp do_compile_parameter_types(%Query{parameters: nil} = query), do:
    # Parameters are not bound. This is possible if PostgreSQL client issues a describe command before it
    # binds parameters, which is allowed by the protocol. In this case, we'll derive parameter types from
    # cast expressions. In other words, with late binding the parameter must be explicitly casted, so we
    # can determine its type.
    query
    |> add_parameter_types_from_subqueries()
    |> add_parameter_types_from_casts()

  defp add_parameter_types_from_subqueries(query), do:
    Enum.reduce(
      get_in(query, [Lenses.direct_subqueries()]),
      query,
      &Query.merge_parameter_types(&2, &1.ast)
    )

  defp add_parameter_types_from_casts(query), do:
    Enum.reduce(
      get_in(query, [Lenses.raw_parameter_casts()]),
      query,
      fn({:function, {:cast, type}, [{:parameter, index}], _location}, query) ->
        Query.set_parameter_type(query, index, type)
      end
    )

  defp check_missing_parameter_types(%Query{subquery?: true} = query), do:
    query
  defp check_missing_parameter_types(%Query{subquery?: false} = query) do
    # This verifies whether there's a missing parameter type. This can happen if a late bound parameter has
    # not been casted, or if it's omitted from the query.
    case Enum.find_index(Query.parameter_types(query), &(&1 == :unknown)) do
      nil -> query
      index -> parameter_error(index + 1)
    end
  end

  defp parameter_error(parameter_index), do:
    raise(CompilationError, message: "The type for parameter `$#{parameter_index}` cannot be determined.")


  # -------------------------------------------------------------------
  # Transformation of columns
  # -------------------------------------------------------------------

  defp expand_star_select(query), do:
    %Query{query | columns: Enum.flat_map(query.columns, &expand_select_all(&1, query))}

  defp expand_select_all(:*, query), do:
    query
    |> all_visible_columns()
    |> columns_to_identifiers()
  defp expand_select_all({:*, table_name}, query) do
    with [] <-
      query
      |> all_visible_columns()
      |> Enum.filter(&(&1.table.name == table_name))
      |> columns_to_identifiers()
    do
      raise CompilationError, message:
        "Select clause `#{table_name}`.* cannot be resolved because the table does not exist in the `FROM` list."
    end
  end
  defp expand_select_all(column, _query), do:
    [column]

  defp all_visible_columns(query), do:
    query.selected_tables
    |> Enum.flat_map(fn(table) -> Enum.map(table.columns, fn(column) -> %{table: table, column: column} end) end)
    |> Enum.filter(&(&1.column.visible?))

  defp columns_to_identifiers(columns), do:
    Enum.map(columns, &{:identifier, &1.table.name, {:unquoted, &1.column.name}, @dummy_location})

  defp compile_aliases(%Query{columns: [_|_] = columns} = query) do
    verify_aliases(query)
    column_titles = Enum.map(columns, &column_title(&1, query.selected_tables))
    aliases = for {column, :as, name} <- columns, into: %{}, do:
      {{:identifier, :unknown, {:unquoted, name}, @dummy_location}, column}
    columns = Enum.map(columns, fn ({column, :as, _name}) -> column; (column) -> column end)
    order_by = for {column, direction} <- query.order_by, do: {resolve_alias(aliases, column), direction}
    group_by = for identifier <- query.group_by, do: resolve_alias(aliases, identifier)
    where = update_in(query.where, [Lenses.conditions_terminals()], &resolve_alias(aliases, &1))
    having = update_in(query.having, [Lenses.conditions_terminals()], &resolve_alias(aliases, &1))
    %Query{query | columns: columns, column_titles: column_titles,
      group_by: group_by, order_by: order_by, where: where, having: having}
  end
  defp compile_aliases(query), do: query

  defp resolve_alias(aliases, identifier = {:identifier, table, column, _loc}), do:
    Map.get(aliases, {:identifier, table, column, @dummy_location}, identifier)
  defp resolve_alias(_aliases, other), do: other

  defp column_title({_identifier, :as, alias}, _selected_tables), do: alias
  defp column_title({:function, {:cast, _}, _, _}, _selected_tables), do: "cast"
  defp column_title({:function, {:bucket, _}, _, _}, _selected_tables), do: "bucket"
  defp column_title({:function, name, _, _}, _selected_tables), do: name
  defp column_title({:distinct, identifier}, selected_tables), do: column_title(identifier, selected_tables)
  # This is needed for data sources that support dotted names for fields (MongoDB)
  defp column_title({:identifier, {:unquoted, table}, {:unquoted, column}, _}, selected_tables), do:
    if find_table(selected_tables, {:unquoted, table}) == nil, do: "#{table}.#{column}", else: column
  defp column_title({:identifier, _table, {_, column}, _}, _selected_tables), do: column
  defp column_title({:constant, _, _, _}, _selected_tables), do: ""
  defp column_title({:parameter, _}, _selected_tables), do: ""

  # Subqueries can produce column-names that are not actually in the table. Without understanding what
  # is being produced by the subquery (currently it is being treated as a blackbox), we cannot validate
  # the outer column selections
  defp verify_aliases(query) do
    aliases = for {_column, :as, name} <- query.columns, do: name
    all_columns = query |> all_visible_columns() |> Enum.map(&(&1.column.name))
    possible_identifiers = aliases ++ all_columns
    referenced_identifiers =
      (for {identifier, _direction} <- query.order_by, do: identifier) ++
      query.group_by ++
      Lens.to_list(Lenses.conditions_terminals(), [query.where, query.having])
    ambiguous_names = for {:identifier, :unknown, {_, name}, _} <- referenced_identifiers,
      Enum.count(possible_identifiers, &name == &1) > 1, do: name
    case ambiguous_names do
      [] -> :ok
      [name | _rest] -> raise CompilationError, message: "Usage of `#{name}` is ambiguous."
    end
  end

  defp compile_columns(query) do
    columns_by_name =
      for table <- query.selected_tables, column <- table.columns do
        Expression.column(column, table)
      end
      |> Enum.group_by(&(&1.name))
    query = map_terminal_elements(query, &normalize_table_name(&1, query.selected_tables))
    map_terminal_elements(query, &identifier_to_column(&1, columns_by_name, query))
  end

  defp map_terminal_elements(query, mapper_fun), do:
    Lens.map(Lenses.terminals(), query, mapper_fun)

  defp normalize_table_name({:identifier, table_identifier = {_, name}, column, location}, selected_tables) do
    case find_table(selected_tables, table_identifier) do
      nil -> {:identifier, name, column, location}
      table -> {:identifier, table.name, column, location}
    end
  end
  defp normalize_table_name(x, _), do: x

  defp identifier_to_column({:identifier, :unknown, identifier = {_, column_name}, _loc}, columns_by_name, _query) do
    case get_columns(columns_by_name, identifier) do
      [column] -> column
      [_|_] -> raise CompilationError, message: "Column `#{column_name}` is ambiguous."
      nil ->
        columns_by_name
        |> Map.values()
        |> List.flatten()
        |> Enum.map(&(&1.table))
        |> Enum.uniq()
        |> case do
            [table] ->
              raise CompilationError, message: "Column `#{column_name}` doesn't exist in table `#{table.name}`."
            [_|_] ->
              raise CompilationError, message: "Column `#{column_name}` doesn't exist in any of the selected tables."
          end
    end
  end
  defp identifier_to_column({:identifier, table, identifier = {_, column_name}, _loc}, columns_by_name, query) do
    if Enum.any?(query.selected_tables, &(&1.name == table)) do
      case get_columns(columns_by_name, identifier) do
        nil ->
          raise CompilationError, message: "Column `#{column_name}` doesn't exist in table `#{table}`."
        columns ->
          case Enum.find(columns, &insensitive_equal?(&1.table.name, table)) do
            nil ->
              raise CompilationError, message: "Column `#{column_name}` doesn't exist in table `#{table}`."
            column -> column
          end
      end
    else
      case get_columns(columns_by_name, {:unquoted, "#{table}.#{column_name}"}) do
        [column] -> column
        [_|_] -> raise CompilationError, message: "Column `#{table}.#{column_name}` is ambiguous."
        nil -> raise CompilationError, message: "Missing FROM clause entry for table `#{table}`."
      end
    end
  end
  defp identifier_to_column({:function, name, args, location} = function, _columns_by_name, query) do
    function
    |> Validation.verify_function(query.subquery?)
    |> Function.return_type()
    |> case do
      nil -> raise CompilationError, message: function_argument_error_message(function)
      type ->
        Expression.function(name, args, type, Function.has_attribute?(name, :aggregator))
        |> Expression.set_location(location)
    end
  end
  defp identifier_to_column({:parameter, index}, _columns_by_name, query) do
    param_value = if query.parameters != nil, do: Enum.at(query.parameters, index - 1).value
    param_type = Query.parameter_type(query, index)
    if param_type == :unknown, do: parameter_error(index)
    Expression.constant(param_type, param_value)
  end
  defp identifier_to_column({:constant, type, value, _location}, _columns_by_name, _query), do:
    Expression.constant(type, value)
  defp identifier_to_column({:like_pattern, {:constant, _, pattern, _}, {:constant, _, escape, _}}, _, _) do
    if escape == nil or String.length(escape) == 1 do
      Expression.like_pattern(pattern, escape)
    else
      raise CompilationError, message: "Escape string must be one character."
    end
  end
  defp identifier_to_column(other, _columns_by_name, _query), do: other

  defp get_columns(columns_by_name, {:unquoted, name}) do
    columns_by_name
    |> Enum.filter(fn({key, _}) -> insensitive_equal?(name, key) end)
    |> Enum.flat_map(fn({_key, columns}) -> columns end)
    |> case do
      [] -> nil
      columns -> columns
    end
  end
  defp get_columns(columns_by_name, {:quoted, name}), do: Map.get(columns_by_name, name)

  defp function_argument_error_message({:function, name, _, _} = function_call) do
    cond do
      Function.cast?(function_call) ->
        [cast_source] = actual_types(function_call)
        cast_target = Function.cast_target(function_call)
        "Cannot cast value of type `#{cast_source}` to type `#{cast_target}`."
      many_overloads?(function_call) ->
        "Arguments of type (#{function_call |> actual_types() |> quoted_types()}) are incorrect"
          <> " for `#{Function.readable_name(name)}`."
      true ->
        "Function `#{Function.readable_name(name)}` requires arguments of type #{expected_types(function_call)}"
          <> ", but got (#{function_call |> actual_types() |> quoted_types()})."
    end
  end

  defp actual_types(function_call), do:
    Function.arguments(function_call)
    |> Enum.map(fn
      (%Expression{} = expression) -> expression.type
      (:*) -> "unspecified type"
    end)

  defp many_overloads?(function_call), do: length(Function.argument_types(function_call)) > 4

  defp quoted_types(items), do: items |> Enum.map(&quoted_type/1) |> Enum.join(", ")

  defp quoted_type({:optional, type}), do: "[`#{type}`]"
  defp quoted_type({:many1, type}), do: "[`#{type}`]+"
  defp quoted_type({:or, types}), do: types |> Enum.map(&quoted_type/1) |> Enum.join(" | ")
  defp quoted_type({:constant, type}), do: "`constant #{type}`"
  defp quoted_type(type), do: "`#{type}`"

  defp expected_types(function_call), do:
    Function.argument_types(function_call)
    |> Enum.map(&quoted_types/1)
    |> Enum.map(&"(#{&1})")
    |> Enum.join(" or ")


  # -------------------------------------------------------------------
  # Resolving of references
  # -------------------------------------------------------------------

  defp compile_references(query), do:
    query
    |> compile_group_by_references()
    |> compile_order_by_references()

  defp compile_group_by_references(query), do:
    %Query{query | group_by: Enum.map(query.group_by, &compile_reference(&1, query, "GROUP BY"))}

  defp compile_order_by_references(query), do:
    %Query{query | order_by:
      Enum.map(
        query.order_by,
        fn({expression, direction}) -> {compile_reference(expression, query, "ORDER BY"), direction} end
      )
    }

  defp compile_reference(%Expression{constant?: true, type: :integer} = reference, query, clause_name) do
    unless reference.value in 1..length(query.columns), do:
      raise(CompilationError,
        message: "`#{clause_name}` position `#{reference.value}` is out of the range of selected columns.")

    Enum.at(query.columns, reference.value - 1)
  end
  defp compile_reference(%Expression{constant?: true, type: _}, _query, clause_name), do:
    raise(CompilationError, message: "Non-integer constant is not allowed in `#{clause_name}`.")
  defp compile_reference(expression, _query, _clause_name), do:
    expression


  # -------------------------------------------------------------------
  # Where clauses
  # -------------------------------------------------------------------

  defp cast_where_clauses(query), do:
    %Query{query | where: Lens.map(Lenses.conditions(), query.where, &cast_where_clause/1)}

  defp cast_where_clause(clause) do
    column = Condition.subject(clause)
    do_cast_where_clause(clause, column.type)
  end

  @castable_conditions [:datetime, :time, :date]

  defp do_cast_where_clause({:comparison, identifier, comparator, rhs}, type) when type in @castable_conditions do
    if Expression.constant?(rhs) do
      {:comparison, identifier, comparator, parse_time(rhs, type)}
    else
      {:comparison, identifier, comparator, rhs}
    end
  end
  defp do_cast_where_clause({:in, column, values}, type) when type in @castable_conditions, do:
    {:in, column, Enum.map(values, &parse_time(&1, type))}
  defp do_cast_where_clause(clause, _), do: clause

  defp parse_time(expression, type) do
    value = Expression.value(expression)

    case do_parse_time(value, type) do
      {:ok, result} -> Expression.constant(type, result)
      _ -> raise CompilationError, message: "Cannot cast `#{value}` to #{type}."
    end
  end

  defp do_parse_time(string, :date) when is_binary(string), do: Cloak.Time.parse_date(string)
  defp do_parse_time(string, :time) when is_binary(string), do: Cloak.Time.parse_time(string)
  defp do_parse_time(string, :datetime) when is_binary(string), do: Cloak.Time.parse_datetime(string)
  defp do_parse_time(_, _), do: {:error, :invalid_cast}


  # -------------------------------------------------------------------
  # UID selection in a subquery
  # -------------------------------------------------------------------

  defp ensure_uid_selected(subquery, alias) do
    case auto_select_uid_column(subquery) do
      nil -> subquery

      :error ->
        possible_uid_columns =
          Helpers.all_id_columns_from_tables(subquery)
          |> Enum.map(&Expression.display_name/1)
          |> case do
            [column] -> "the column #{column}"
            columns -> "one of the columns #{Enum.join(columns, ", ")}"
          end
        raise CompilationError, message:
          "Missing a user id column in the select list of #{"subquery `#{alias}`"}. " <>
          "To fix this error, add #{possible_uid_columns} to the subquery select list."

      uid_column ->
        uid_alias = "__auto_selected_#{uid_column.table.name}.#{uid_column.name}__"
        selected_expression = %Expression{uid_column | alias: uid_alias, synthetic?: true}
        %Query{subquery |
          columns: subquery.columns ++ [selected_expression],
          column_titles: subquery.column_titles ++ [uid_alias]
        }
    end
  end

  defp auto_select_uid_column(subquery) do
    cond do
      # uid column is already explicitly selected
      Helpers.uid_column_selected?(subquery) -> nil

      # no group by and no aggregate -> select any uid column
      match?(%Query{group_by: []}, subquery) && not Helpers.aggregate?(subquery) ->
        hd(Helpers.all_id_columns_from_tables(subquery))

      # uid column is in a group by -> select that uid
      (uid_column = Enum.find(subquery.group_by, &(&1.user_id?))) != nil ->
        uid_column

      # we can't select a uid column
      true -> :error
    end
  end
end

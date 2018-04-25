defmodule Cloak.Sql.Compiler.Specification do
  @moduledoc "Turns a parsed SQL AST into a `Cloak.Sql.Query` specification describing the user query."

  alias Cloak.DataSource
  alias Cloak.Sql.{Condition, CompilationError, Expression, Function, Query}
  alias Cloak.Sql.{Compiler.Helpers, Query.Lenses}

  @dummy_location {1, 0}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Compiles the parsed query."
  @spec compile(
          Parser.parsed_query(),
          DataSource.t(),
          [Query.parameter()] | nil,
          Query.view_map()
        ) :: Query.t()
  def compile(parsed_query, data_source, parameters, views),
    do:
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

  defp compile_query(%Query{command: :show, show: :tables} = query),
    do: %Query{
      query
      | column_titles: ["name"],
        columns: [%Expression{table: :unknown, constant?: true, name: "name", type: :text}]
    }

  defp compile_query(%Query{command: :show, show: :columns} = query),
    do: %Query{
      compile_from(query)
      | column_titles: ["name", "type"],
        columns:
          Enum.map(
            ["name", "type"],
            &%Expression{table: :unknown, constant?: true, name: &1, type: :text}
          )
    }

  defp compile_query(%Query{command: :select} = query),
    do:
      query
      |> compile_from()
      |> compile_parameter_types()
      |> expand_star_select()
      |> compile_titles()
      |> compile_aliases()
      |> compile_columns()
      |> compile_references()
      |> perform_implicit_casts()

  # -------------------------------------------------------------------
  # From
  # -------------------------------------------------------------------

  defp compile_from(query),
    do:
      query
      |> compile_views()
      |> normalize_from()
      |> compile_subqueries()
      |> compile_selected_tables()

  defp normalize_from(%Query{} = query),
    do:
      query
      |> collect_table_aliases()
      |> update_in([Lenses.ast_tables()], &normalize_from(&1, query.data_source))

  defp collect_table_aliases(query) do
    aliases =
      query
      |> get_in([Lenses.ast_tables()])
      |> Enum.filter(&match?({_table_identifier, :as, _alias}, &1))
      |> Enum.map(fn {table_identifier, :as, alias} ->
        {alias, find_table!(query.data_source, table_identifier)}
      end)

    verify_duplicate_aliases(aliases)

    %Query{query | table_aliases: Map.new(aliases)}
  end

  defp verify_duplicate_aliases(aliases) do
    aliases
    |> Enum.map(fn {alias, _} -> alias end)
    |> Enum.group_by(& &1)
    |> Enum.reject(&match?({_alias, [_]}, &1))
    |> Enum.map(fn {alias, _} -> alias end)
    |> case do
      [duplicate_alias | _] ->
        raise CompilationError, message: "Table alias `#{duplicate_alias}` used more than once."

      [] ->
        :ok
    end
  end

  defp normalize_from({_table_identifier, :as, alias}, _data_source), do: alias

  defp normalize_from(table_identifier, data_source), do: find_table!(data_source, table_identifier).name

  defp find_table!(data_source, table_identifier = {_, table_name}) do
    case data_source |> DataSource.tables() |> find_table(table_identifier) do
      nil -> raise CompilationError, message: "Table `#{table_name}` doesn't exist."
      table -> table
    end
  end

  defp find_table(tables, {:quoted, name}), do: Enum.find(tables, &(name == &1.name))

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
    {:join, %{join | lhs: do_compile_views(join.lhs, query), rhs: do_compile_views(join.rhs, query)}}
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

  defp do_compile_views({:subquery, subquery}, _query), do: {:subquery, subquery}

  defp view_to_subquery(view_name, view_sql, query) do
    if Enum.any?(query.data_source.tables, fn {_id, table} ->
         insensitive_equal?(table.name, view_name)
       end) do
      raise CompilationError,
        message: "There is both a table, and a view named `#{view_name}`. Rename the view to resolve the conflict."
    end

    case Cloak.Sql.Parser.parse(view_sql) do
      {:ok, parsed_view} ->
        {:subquery, %{ast: Map.put(parsed_view, :view?, true), alias: view_name}}

      {:error, error} ->
        raise CompilationError, message: "Error in the view `#{view_name}`: #{error}"
    end
  end

  # -------------------------------------------------------------------
  # Subqueries
  # -------------------------------------------------------------------

  defp compile_subqueries(query),
    do:
      Lens.map(
        Lenses.direct_subqueries(),
        query,
        &%{&1 | ast: compile_subquery(&1.ast, &1.alias, query)}
      )

  defp compile_subquery(parsed_subquery, alias, parent_query),
    do:
      parsed_subquery
      |> Map.put(:subquery?, true)
      |> compile(parent_query.data_source, parent_query.parameters, parent_query.views)
      |> ensure_uid_selected(alias)

  # -------------------------------------------------------------------
  # Selected tables
  # -------------------------------------------------------------------

  defp compile_selected_tables(query), do: %Query{query | selected_tables: selected_tables(query.from, query)}

  defp selected_tables({:join, join}, query), do: selected_tables(join.lhs, query) ++ selected_tables(join.rhs, query)

  defp selected_tables({:subquery, subquery}, _query) do
    user_id_name =
      if subquery.ast.type == :standard do
        nil
      else
        # In a subquery we should have the `user_id` already in the list of selected columns.
        user_id_index = Enum.find_index(subquery.ast.columns, & &1.user_id?)
        Enum.at(subquery.ast.column_titles, user_id_index)
      end

    columns =
      Enum.zip(subquery.ast.column_titles, subquery.ast.columns)
      |> Enum.map(fn {alias, column} ->
        DataSource.Table.column(alias, Function.type(column), visible?: not column.synthetic?)
      end)

    keys =
      Enum.zip(subquery.ast.column_titles, subquery.ast.columns)
      |> Enum.filter(fn {_, column} -> Expression.key?(column) end)
      |> Enum.map(fn {title, _} -> title end)

    [DataSource.Table.new(subquery.alias, user_id_name, columns: columns, keys: keys)]
  end

  defp selected_tables(table_name, query) when is_binary(table_name),
    do: [%{table_from_name_or_alias!(query, table_name) | name: table_name}]

  defp table_from_name_or_alias!(query, table_name) do
    table = Map.get(query.table_aliases, table_name, DataSource.table(query.data_source, table_name))

    true = table != nil
    table
  end

  # -------------------------------------------------------------------
  # Parameter types
  # -------------------------------------------------------------------

  defp compile_parameter_types(query),
    do:
      query
      |> do_compile_parameter_types()
      |> check_missing_parameter_types()

  defp do_compile_parameter_types(%Query{parameters: parameters} = query)
       when is_list(parameters),
       # Parameters are bound
       do:
         parameters
         |> Enum.with_index()
         |> Enum.reduce(query, fn {param, index}, query ->
           Query.set_parameter_type(query, index + 1, param.type)
         end)

  defp do_compile_parameter_types(%Query{parameters: nil} = query),
    # Parameters are not bound. This is possible if PostgreSQL client issues a describe command before it
    # binds parameters, which is allowed by the protocol. In this case, we'll derive parameter types from
    # cast expressions. In other words, with late binding the parameter must be explicitly casted, so we
    # can determine its type.
    do:
      query
      |> add_parameter_types_from_subqueries()
      |> add_parameter_types_from_casts()

  defp add_parameter_types_from_subqueries(query),
    do:
      Enum.reduce(
        get_in(query, [Lenses.direct_subqueries()]),
        query,
        &Query.merge_parameter_types(&2, &1.ast)
      )

  defp add_parameter_types_from_casts(query),
    do:
      Enum.reduce(get_in(query, [Lenses.raw_parameter_casts()]), query, fn {:function, {:cast, type},
                                                                            [{:parameter, index}], _location},
                                                                           query ->
        Query.set_parameter_type(query, index, type)
      end)

  defp check_missing_parameter_types(%Query{subquery?: true} = query), do: query

  defp check_missing_parameter_types(%Query{subquery?: false} = query) do
    # This verifies whether there's a missing parameter type. This can happen if a late bound parameter has
    # not been casted, or if it's omitted from the query.
    case Enum.find_index(Query.parameter_types(query), &(&1 == :unknown)) do
      nil -> query
      index -> parameter_error(index + 1)
    end
  end

  defp parameter_error(parameter_index),
    do:
      raise(
        CompilationError,
        message: "The type for parameter `$#{parameter_index}` cannot be determined."
      )

  # -------------------------------------------------------------------
  # Transformation of columns
  # -------------------------------------------------------------------

  defp expand_star_select(query),
    do: %Query{query | columns: Enum.flat_map(query.columns, &(&1 |> expand_select_all(query) |> verify_star_select()))}

  defp expand_select_all(:*, query),
    do:
      query
      |> all_visible_columns()
      |> columns_to_identifiers()

  defp expand_select_all({:*, table_name}, query) do
    with [] <-
           query
           |> all_visible_columns()
           |> Enum.filter(&(&1.table.name == table_name))
           |> columns_to_identifiers() do
      raise CompilationError,
        message:
          "Select clause `#{table_name}`.* cannot be resolved because the table does not exist in the `FROM` list."
    end
  end

  defp expand_select_all(column, _query), do: [column]

  defp all_visible_columns(query),
    do:
      query.selected_tables
      |> Enum.flat_map(fn table ->
        Enum.map(table.columns, fn column -> %{table: table, column: column} end)
      end)
      |> Enum.filter(& &1.column.visible?)

  defp columns_to_identifiers(columns),
    do:
      Enum.map(
        columns,
        &{:identifier, &1.table.name, {:unquoted, &1.column.name}, @dummy_location}
      )

  defp verify_star_select(columns) do
    case columns -- Enum.uniq(columns) do
      [] ->
        columns

      [{:identifier, table_name, {:unquoted, column_name}, _} | _] ->
        raise CompilationError,
          message:
            "Selecting all from subquery `#{table_name}` is not supported" <>
              " because the column name `#{column_name}` is ambigous."
    end
  end

  defp compile_titles(%Query{subquery?: true} = query) do
    column_titles =
      query.columns
      |> Enum.map(&column_title(&1, query.selected_tables))
      |> Enum.with_index(1)
      |> Enum.map(fn {title, index} -> title || "__ac_??#{index}" end)

    %Query{query | column_titles: column_titles}
  end

  defp compile_titles(%Query{subquery?: false} = query) do
    column_titles =
      query.columns
      |> Enum.map(&column_title(&1, query.selected_tables))
      |> Enum.map(fn
        "__ac_??" <> _ -> ""
        title -> title || ""
      end)

    %Query{query | column_titles: column_titles}
  end

  defp compile_aliases(%Query{columns: [_ | _] = columns} = query) do
    verify_aliases(query)

    aliases =
      for {column, :as, name} <- columns,
          into: %{},
          do: {{:identifier, :unknown, {:unquoted, name}, @dummy_location}, column}

    columns =
      Enum.map(columns, fn
        {column, :as, _name} -> column
        column -> column
      end)

    order_by = for {column, direction, nulls} <- query.order_by, do: {resolve_alias(aliases, column), direction, nulls}

    group_by = for identifier <- query.group_by, do: resolve_alias(aliases, identifier)
    where = update_in(query.where, [Lenses.conditions_terminals()], &resolve_alias(aliases, &1))
    having = update_in(query.having, [Lenses.conditions_terminals()], &resolve_alias(aliases, &1))

    %Query{
      query
      | columns: columns,
        group_by: group_by,
        order_by: order_by,
        where: where,
        having: having
    }
  end

  defp compile_aliases(query), do: query

  defp resolve_alias(aliases, identifier = {:identifier, table, column, _loc}),
    do: Map.get(aliases, {:identifier, table, column, @dummy_location}, identifier)

  defp resolve_alias(_aliases, other), do: other

  defp column_title({_identifier, :as, alias}, _selected_tables), do: alias

  defp column_title({:function, {:cast, _}, [expression], _}, selected_tables),
    do: column_title(expression, selected_tables)

  defp column_title({:function, name, _, _}, _selected_tables), do: Function.readable_name(name)

  defp column_title({:distinct, identifier}, selected_tables), do: column_title(identifier, selected_tables)

  # This is needed for data sources that support dotted names for fields (MongoDB)
  defp column_title({:identifier, {:unquoted, table}, {:unquoted, column}, _}, selected_tables),
    do:
      if(
        find_table(selected_tables, {:unquoted, table}) == nil,
        do: "#{table}.#{column}",
        else: column
      )

  defp column_title({:identifier, _table, {_, column}, _}, _selected_tables), do: column
  defp column_title({:constant, _, _, _}, _selected_tables), do: nil
  defp column_title({:parameter, _}, _selected_tables), do: nil

  # Subqueries can produce column-names that are not actually in the table. Without understanding what
  # is being produced by the subquery (currently it is being treated as a blackbox), we cannot validate
  # the outer column selections
  defp verify_aliases(query) do
    aliases = for {_column, :as, name} <- query.columns, do: name
    all_columns = query |> all_visible_columns() |> Enum.map(& &1.column.name)
    possible_identifiers = aliases ++ all_columns

    referenced_identifiers =
      Query.order_by_expressions(query) ++
        query.group_by ++ Lens.to_list(Lenses.conditions_terminals(), [query.where, query.having])

    ambiguous_names =
      for {:identifier, :unknown, {_, name}, location} <- referenced_identifiers,
          Enum.count(possible_identifiers, &(name == &1)) > 1,
          do: {name, location}

    case ambiguous_names do
      [] ->
        :ok

      [{name, location} | _rest] ->
        raise CompilationError,
          source_location: location,
          message: "Usage of `#{name}` is ambiguous."
    end
  end

  defp compile_columns(query) do
    columns_by_name =
      for table <- query.selected_tables,
          column <- table.columns do
        Expression.column(column, table)
      end
      |> Enum.group_by(& &1.name)

    query = map_terminal_elements(query, &normalize_table_name(&1, query.selected_tables))
    map_terminal_elements(query, &identifier_to_column(&1, columns_by_name, query))
  end

  defp map_terminal_elements(query, mapper_fun), do: Lens.map(Lenses.terminals(), query, mapper_fun)

  defp normalize_table_name(
         {:identifier, table_identifier = {_, name}, column, location},
         selected_tables
       ) do
    case find_table(selected_tables, table_identifier) do
      nil -> {:identifier, name, column, location}
      table -> {:identifier, table.name, column, location}
    end
  end

  defp normalize_table_name(x, _), do: x

  defp identifier_to_column(
         {:identifier, :unknown, identifier = {_, column_name}, loc},
         columns_by_name,
         _query
       ) do
    case get_columns(columns_by_name, identifier) do
      [column] ->
        column

      [_ | _] ->
        raise CompilationError,
          source_location: loc,
          message: "Column `#{column_name}` is ambiguous."

      nil ->
        columns_by_name
        |> Map.values()
        |> List.flatten()
        |> Enum.map(& &1.table)
        |> Enum.uniq()
        |> case do
          [table] ->
            raise CompilationError,
              message: "Column `#{column_name}` doesn't exist in table `#{table.name}`.",
              source_location: loc

          [_ | _] ->
            raise CompilationError,
              message: "Column `#{column_name}` doesn't exist in any of the selected tables.",
              source_location: loc
        end
    end
    |> Expression.set_location(loc)
  end

  defp identifier_to_column(
         {:identifier, table, identifier = {_, column_name}, loc},
         columns_by_name,
         query
       ) do
    if Enum.any?(query.selected_tables, &(&1.name == table)) do
      case get_columns(columns_by_name, identifier) do
        nil ->
          raise CompilationError,
            source_location: loc,
            message: "Column `#{column_name}` doesn't exist in table `#{table}`."

        columns ->
          case Enum.find(columns, &insensitive_equal?(&1.table.name, table)) do
            nil ->
              raise CompilationError,
                source_location: loc,
                message: "Column `#{column_name}` doesn't exist in table `#{table}`."

            column ->
              column
          end
      end
    else
      case get_columns(columns_by_name, {:unquoted, "#{table}.#{column_name}"}) do
        [column] ->
          column

        [_ | _] ->
          raise CompilationError,
            source_location: loc,
            message: "Column `#{table}.#{column_name}` is ambiguous."

        nil ->
          raise CompilationError,
            source_location: loc,
            message: "Missing FROM clause entry for table `#{table}`."
      end
    end
    |> Expression.set_location(loc)
  end

  defp identifier_to_column({:function, name, args, location} = function, _columns_by_name, query) do
    function
    |> verify_function_exists(query.type)
    |> Function.return_type()
    |> case do
      nil ->
        raise CompilationError,
          source_location: location,
          message: function_argument_error_message(function)

      type ->
        Expression.function(
          Function.canonical_name(name),
          args,
          type,
          Function.has_attribute?(name, :aggregator)
        )
    end
    |> Expression.set_location(location)
  end

  defp identifier_to_column({:parameter, index}, _columns_by_name, query) do
    param_value = if query.parameters != nil, do: Enum.at(query.parameters, index - 1).value
    param_type = Query.parameter_type(query, index)
    if param_type == :unknown, do: parameter_error(index)
    Expression.constant(param_type, param_value)
  end

  defp identifier_to_column({:constant, type, value, location}, _columns_by_name, _query),
    do: Expression.constant(type, value) |> Expression.set_location(location)

  defp identifier_to_column(
         {:like_pattern, {:constant, _, pattern, location}, {:constant, _, escape, _}},
         _,
         _
       ) do
    if escape == nil or String.length(escape) == 1 do
      Expression.like_pattern(pattern, escape)
    else
      raise CompilationError,
        source_location: location,
        message: "Escape string must be one character."
    end
    |> Expression.set_location(location)
  end

  defp identifier_to_column(other, _columns_by_name, _query), do: other

  defp get_columns(columns_by_name, {:unquoted, name}) do
    columns_by_name
    |> Enum.filter(fn {key, _} -> insensitive_equal?(name, key) end)
    |> Enum.flat_map(fn {_key, columns} -> columns end)
    |> case do
      [] -> nil
      columns -> columns
    end
  end

  defp get_columns(columns_by_name, {:quoted, name}), do: Map.get(columns_by_name, name)

  defp verify_function_exists(function = {:function, name, _, location}, query_type) do
    unless Function.exists?(function) and (query_type == :standard or not Function.internal?(function)) do
      case Function.deprecation_info(function) do
        {:error, error} when error in [:not_found, :internal_function] ->
          raise CompilationError,
            source_location: location,
            message: "Unknown function `#{Function.readable_name(name)}`."

        {:ok, %{alternative: alternative}} ->
          raise CompilationError,
            source_location: location,
            message:
              "Function `#{Function.readable_name(name)}` has been deprecated. " <>
                "Depending on your use case, consider using `#{Function.readable_name(alternative)}` instead."
      end
    end

    function
  end

  defp function_argument_error_message({:function, name, _, _} = function_call) do
    cond do
      Function.cast?(function_call) ->
        [cast_source] = actual_types(function_call)
        cast_target = Function.cast_target(function_call)
        "Cannot cast value of type `#{cast_source}` to type `#{cast_target}`."

      many_overloads?(function_call) ->
        "Arguments of type (#{function_call |> actual_types() |> quoted_types()}) are incorrect" <>
          " for `#{Function.readable_name(name)}`."

      true ->
        "Function `#{Function.readable_name(name)}` requires arguments of type #{expected_types(function_call)}" <>
          ", but got (#{function_call |> actual_types() |> quoted_types()})."
    end
  end

  defp actual_types(function_call),
    do:
      Function.arguments(function_call)
      |> Enum.map(fn
        %Expression{} = expression -> expression.type
        :* -> "unspecified type"
      end)

  defp many_overloads?(function_call), do: length(Function.argument_types(function_call)) > 4

  defp quoted_types(items), do: items |> Enum.map(&quoted_type/1) |> Enum.join(", ")

  defp quoted_type({:optional, type}), do: "[" <> quoted_type(type) <> "]"
  defp quoted_type({:many1, type}), do: "[" <> quoted_type(type) <> "]+"
  defp quoted_type({:or, types}), do: types |> Enum.map(&quoted_type/1) |> Enum.join(" | ")
  defp quoted_type({:constant, type}), do: "`constant #{type}`"
  defp quoted_type(type), do: "`#{type}`"

  defp expected_types(function_call),
    do:
      Function.argument_types(function_call)
      |> Enum.map(&quoted_types/1)
      |> Enum.map(&"(#{&1})")
      |> Enum.join(" or ")

  # -------------------------------------------------------------------
  # Resolving of references
  # -------------------------------------------------------------------

  defp compile_references(query),
    do:
      query
      |> compile_group_by_references()
      |> compile_order_by_references()

  defp compile_group_by_references(query),
    do: %Query{
      query
      | group_by: Enum.map(query.group_by, &compile_reference(&1, query, "GROUP BY"))
    }

  defp compile_order_by_references(query),
    do: %Query{
      query
      | order_by:
          Enum.map(query.order_by, fn {expression, direction, nulls} ->
            {compile_reference(expression, query, "ORDER BY"), direction, nulls}
          end)
    }

  defp compile_reference(
         %Expression{constant?: true, type: :integer} = reference,
         query,
         clause_name
       ) do
    unless reference.value in 1..length(query.columns),
      do:
        raise(
          CompilationError,
          source_location: reference.source_location,
          message: "`#{clause_name}` position `#{reference.value}` is out of the range of selected columns."
        )

    Enum.at(query.columns, reference.value - 1)
  end

  defp compile_reference(%Expression{constant?: true, type: _} = constant, _query, clause_name),
    do:
      raise(
        CompilationError,
        source_location: constant.source_location,
        message: "Non-integer constant is not allowed in `#{clause_name}`."
      )

  defp compile_reference(expression, _query, _clause_name), do: expression

  # -------------------------------------------------------------------
  # Implicit casts
  # -------------------------------------------------------------------

  defp perform_implicit_casts(query) do
    Lenses.filter_clauses()
    |> Lenses.conditions()
    |> Lens.map(query, fn clause ->
      column = Condition.subject(clause)
      perform_implicit_cast(clause, column.type)
    end)
  end

  @castable_conditions [:datetime, :time, :date]

  defp perform_implicit_cast({:comparison, identifier, comparator, rhs}, type) when type in @castable_conditions do
    if Expression.constant?(rhs) do
      {:comparison, identifier, comparator, parse_time(rhs, type)}
    else
      {:comparison, identifier, comparator, rhs}
    end
  end

  defp perform_implicit_cast({:in, column, values}, type) when type in @castable_conditions,
    do: {:in, column, Enum.map(values, &parse_time(&1, type))}

  defp perform_implicit_cast(clause, _), do: clause

  defp parse_time(expression = %Expression{type: type}, type), do: expression

  defp parse_time(expression, type) do
    value = Expression.value(expression)

    case do_parse_time(value, type) do
      {:ok, result} ->
        Expression.constant(type, result)

      _ ->
        raise CompilationError,
          source_location: expression.source_location,
          message: "Cannot cast `#{value}` to #{type}."
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
      nil ->
        subquery

      :error ->
        possible_uid_columns =
          Helpers.all_id_columns_from_tables(subquery)
          |> Enum.map(&Expression.display_name/1)
          |> case do
            [column] -> "the column #{column}"
            columns -> "one of the columns #{Enum.join(columns, ", ")}"
          end

        raise CompilationError,
          message:
            "Missing a user id column in the select list of #{"subquery `#{alias}`"}. " <>
              "To fix this error, add #{possible_uid_columns} to the subquery select list."

      uid_column ->
        uid_alias = "__auto_selected_#{uid_column.table.name}.#{uid_column.name}__"
        selected_expression = %Expression{uid_column | alias: uid_alias, synthetic?: true}

        %Query{
          subquery
          | columns: subquery.columns ++ [selected_expression],
            column_titles: subquery.column_titles ++ [uid_alias]
        }
    end
  end

  defp auto_select_uid_column(subquery) do
    cond do
      # virtual table queries don't require user ids
      subquery.type == :standard ->
        nil

      # uid column is already explicitly selected
      Helpers.uid_column_selected?(subquery) ->
        nil

      # no group by, no having and no aggregate -> select any uid column
      match?(%Query{group_by: [], having: nil}, subquery) && not Helpers.aggregate?(subquery) ->
        hd(Helpers.all_id_columns_from_tables(subquery))

      # uid column is in a group by -> select that uid
      (uid_column = Enum.find(subquery.group_by, & &1.user_id?)) != nil ->
        uid_column

      # we can't select a uid column
      true ->
        :error
    end
  end
end

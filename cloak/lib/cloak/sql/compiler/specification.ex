defmodule Cloak.Sql.Compiler.Specification do
  @moduledoc "Turns a parsed SQL AST into a `Cloak.Sql.Query` specification describing the user query."

  alias Cloak.DataSource
  alias Cloak.Sql.{Condition, CompilationError, Expression, Function, Query}
  alias Cloak.Sql.{Compiler.Helpers, Query.Lenses}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Compiles the parsed query."
  @spec compile(
          Parser.parsed_query(),
          Query.analyst_id(),
          DataSource.t(),
          [Query.parameter()] | nil,
          Query.user_views()
        ) :: Query.t()
  def compile(parsed_query, analyst_id, data_source, parameters, views) do
    %Query{
      analyst_id: analyst_id,
      data_source: data_source,
      available_tables: DataSource.tables(data_source),
      analyst_tables:
        Cloak.AnalystTable.analyst_tables(analyst_id, data_source) |> Stream.map(&{&1.name, &1}) |> Map.new(),
      parameters: cast_parameters(parameters),
      views: views
    }
    |> Map.merge(parsed_query)
    |> compile_query()
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp cast_parameters(nil), do: nil
  defp cast_parameters(parameters), do: Enum.map(parameters, &cast_parameter/1)

  defp cast_parameter(parameter = %{value: nil}), do: parameter

  defp cast_parameter(parameter = %{type: type, value: value}) when type in [:date, :datetime, :time, :interval] do
    case parse_parameter(type, value) do
      {:ok, result} ->
        %{parameter | value: result}

      {:error, reason} ->
        raise CompilationError, message: invalid_literal_error_message(type, value, reason)
    end
  end

  defp cast_parameter(other), do: other

  defp parse_parameter(:date, value), do: Date.from_iso8601(value)
  defp parse_parameter(:datetime, value), do: NaiveDateTime.from_iso8601(value)
  defp parse_parameter(:time, value), do: Time.from_iso8601(value)
  defp parse_parameter(:interval, value), do: Timex.Duration.parse(value)

  @table_attributes ["name", "type", "comment"]
  defp compile_query(%Query{command: :show, show: :tables} = query),
    do: %Query{
      query
      | column_titles: @table_attributes,
        columns: Enum.map(@table_attributes, &Expression.constant(:text, &1))
    }

  @column_attributes ["name", "data type", "isolator?", "key type", "comment"]
  defp compile_query(%Query{command: :show, show: :columns} = query),
    do: %Query{
      compile_from(query)
      | column_titles: @column_attributes,
        columns: Enum.map(@column_attributes, &Expression.constant(:text, &1))
    }

  defp compile_query(%Query{command: :select} = query),
    do:
      query
      |> compile_from()
      |> compile_parameter_types()
      |> expand_star_select()
      |> compile_titles()
      |> compile_grouping_sets()
      |> compile_aliases()
      |> compile_columns()
      |> compile_references()
      |> perform_implicit_casts()
      |> ensure_uid_selected()
      |> Helpers.apply_bottom_up(&collect_required_analyst_tables/1)

  defp compile_query(%Query{command: :union} = query) do
    query
    # We set the default type for unioned subqueries as `standard` to prevent automatic selection of the user id.
    # This will be overwritten later in the pipeline by the `Anonymization` module.
    |> put_in([Lenses.direct_subqueries() |> Lens.key(:ast) |> Lens.key(:type)], :standard)
    |> compile_subqueries()
    |> compile_titles()
    |> compile_columns()
  end

  # -------------------------------------------------------------------
  # From
  # -------------------------------------------------------------------

  defp compile_from(query),
    do:
      query
      |> resolve_views_and_analyst_tables()
      |> normalize_from()
      |> compile_subqueries()
      |> compile_selected_tables()

  defp normalize_from(%Query{} = query),
    do:
      query
      |> collect_table_aliases()
      |> update_in([Lenses.ast_tables()], &normalize_from(&1, query))

  defp collect_table_aliases(query) do
    aliases =
      query
      |> get_in([Lenses.ast_tables()])
      |> Enum.filter(&match?({_table_identifier, :as, _alias}, &1))
      |> Enum.map(fn {table_identifier, :as, alias} ->
        {alias, find_table!(query, table_identifier)}
      end)

    verify_duplicate_aliases(query, aliases)

    %Query{query | table_aliases: Map.new(aliases)}
  end

  defp verify_duplicate_aliases(query, aliases) do
    aliases
    |> Enum.map(fn {alias, _} -> alias end)
    |> Enum.group_by(& &1)
    |> Enum.reject(&match?({_alias, [_]}, &1))
    |> Enum.map(fn {alias, _} -> alias end)
    |> case do
      [duplicate_alias | _] ->
        raise CompilationError,
          source_location: Query.source_location(query),
          message: "Table alias `#{duplicate_alias}` used more than once."

      [] ->
        :ok
    end
  end

  defp normalize_from({_table_identifier, :as, alias}, _query), do: alias

  defp normalize_from(table_identifier, query), do: find_table!(query, table_identifier).name

  defp find_table!(query, table_identifier = {_, table_name}) do
    case find_table(query.available_tables, table_identifier) do
      nil ->
        raise CompilationError,
          source_location: Query.source_location(query),
          message: "Table `#{table_name}` doesn't exist."

      table ->
        table
    end
  end

  defp find_table(tables, {:quoted, name}),
    do: Enum.find(tables, &(strip_public_schema(name) == strip_public_schema(&1.name)))

  defp find_table(tables, {:unquoted, name}),
    do: Enum.find(tables, &insensitive_equal?(strip_public_schema(name), strip_public_schema(&1.name)))

  # `public.some_table` is treated as `some_table`. This is done because clients using Air's PostgreSQL interface might
  # use the `public.` prefix to reference tables.
  defp strip_public_schema("public." <> table_name), do: table_name
  defp strip_public_schema(table_name), do: table_name

  defp insensitive_equal?(s1, s2), do: String.downcase(s1) == String.downcase(s2)

  # -------------------------------------------------------------------
  # Views and analyst tables
  # -------------------------------------------------------------------

  defp resolve_views_and_analyst_tables(query) do
    compiled = do_resolve_views_and_analyst_tables(query.from, query)
    %Query{query | from: compiled}
  end

  defp do_resolve_views_and_analyst_tables({:join, join}, query) do
    {:join,
     %{
       join
       | lhs: do_resolve_views_and_analyst_tables(join.lhs, query),
         rhs: do_resolve_views_and_analyst_tables(join.rhs, query)
     }}
  end

  defp do_resolve_views_and_analyst_tables({reference, :as, alias} = aliased_reference, query) do
    case resolve_leaf_reference(reference, alias, query) do
      {:subquery, _} = subquery -> subquery
      ^reference -> aliased_reference
    end
  end

  defp do_resolve_views_and_analyst_tables({:subquery, subquery}, _query), do: {:subquery, subquery}

  defp do_resolve_views_and_analyst_tables(reference, query), do: resolve_leaf_reference(reference, query)

  defp resolve_leaf_reference({_, name} = reference, alias \\ nil, query) when is_binary(name) do
    alias = alias || name

    case Map.fetch(query.views, name) do
      {:ok, view} ->
        view_to_subquery(name, view, alias, query)

      :error ->
        case Map.fetch(query.analyst_tables, name) do
          {:ok, analyst_table} -> analyst_table_to_subquery(analyst_table, alias)
          :error -> reference
        end
    end
  end

  defp view_to_subquery(view_name, view, alias, query) do
    if Enum.any?(query.data_source.tables, fn {_id, table} -> insensitive_equal?(table.name, view_name) end) do
      raise CompilationError,
        source_location: Query.source_location(query),
        message: "There is both a table, and a view named `#{view_name}`. Rename the view to resolve the conflict."
    end

    case Cloak.Sql.Parser.parse(view.sql) do
      {:ok, parsed_view} -> {:subquery, %{ast: Map.put(parsed_view, :view?, true), alias: alias}}
      {:error, error} -> raise CompilationError, message: "Error in the view `#{view_name}`: #{error}"
    end
  end

  defp analyst_table_to_subquery(analyst_table, alias) do
    case Cloak.Sql.Parser.parse(analyst_table.statement) do
      {:ok, parsed_table} ->
        aliased_table = %{analyst_table | name: alias}
        {:subquery, %{ast: Map.put(parsed_table, :analyst_table, aliased_table), alias: aliased_table.name}}

      {:error, error} ->
        raise CompilationError, message: "Error in the analyst table `#{analyst_table.name}`: #{error}"
    end
  end

  defp collect_required_analyst_tables(query) do
    from_subqueries =
      Lenses.direct_subqueries()
      |> Lens.key(:ast)
      |> Lens.to_list(query)
      |> Enum.map(& &1.required_analyst_tables)

    in_this_query =
      Lenses.direct_subqueries()
      |> Lens.key(:ast)
      |> Lens.filter(&(not is_nil(&1.analyst_table)))
      |> Lens.key(:analyst_table)
      |> Lens.key(:name)
      |> Lens.to_list(query)
      |> MapSet.new()

    %{query | required_analyst_tables: Enum.reduce(from_subqueries, in_this_query, &MapSet.union/2)}
  end

  # -------------------------------------------------------------------
  # Subqueries
  # -------------------------------------------------------------------

  defp compile_subqueries(query) do
    Lens.map(
      Lenses.direct_subqueries(),
      query,
      fn subquery ->
        update_in(
          subquery.ast,
          &(&1
            |> Map.put(:subquery?, true)
            |> compile(query.analyst_id, query.data_source, query.parameters, query.views))
        )
      end
    )
  end

  # -------------------------------------------------------------------
  # Selected tables
  # -------------------------------------------------------------------

  defp compile_selected_tables(query), do: %Query{query | selected_tables: selected_tables(query.from, query)}

  defp selected_tables({:join, join}, query), do: selected_tables(join.lhs, query) ++ selected_tables(join.rhs, query)

  defp selected_tables({:subquery, subquery}, _query) do
    [
      Enum.zip(subquery.ast.column_titles, subquery.ast.columns)
      |> Enum.map(fn {title, column} -> %Expression{column | alias: title} end)
      |> Helpers.create_table_from_columns(subquery.alias, type: :subquery)
    ]
  end

  defp selected_tables(table_name, query) when is_binary(table_name),
    do: [%{table_from_name_or_alias!(query, table_name) | name: table_name}]

  defp table_from_name_or_alias!(query, table_name) do
    table =
      Map.get_lazy(
        query.table_aliases,
        table_name,
        fn -> Enum.find(query.available_tables, &(&1.name == table_name)) end
      )

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
      index -> parameter_error(query, index + 1)
    end
  end

  defp parameter_error(query, parameter_index) do
    raise CompilationError,
      source_location: Query.source_location(query),
      message: "The type for parameter `$#{parameter_index}` cannot be determined."
  end

  # -------------------------------------------------------------------
  # Transformation of columns
  # -------------------------------------------------------------------

  defp expand_star_select(query) do
    columns = Enum.flat_map(query.columns, &(&1 |> expand_select_all(query) |> verify_star_select(query)))
    %Query{query | columns: columns}
  end

  defp expand_select_all({:*, location}, query),
    do:
      query
      |> all_visible_columns()
      |> columns_to_identifiers(location)

  defp expand_select_all({{:*, table_name}, location}, query) do
    with [] <-
           query
           |> all_visible_columns()
           |> Enum.filter(&(&1.table.name == table_name))
           |> columns_to_identifiers(location) do
      raise CompilationError,
        source_location: location,
        message:
          "Select clause `#{table_name}.*` cannot be resolved because the table does not exist in the `FROM` list."
    end
  end

  defp expand_select_all(column, _query), do: [column]

  defp all_visible_columns(query) do
    query.selected_tables
    |> Enum.flat_map(fn table ->
      Enum.map(table.columns, fn column -> %{table: table, column: column} end)
    end)
    |> Enum.filter(&(&1.column.access != :hidden))
  end

  defp columns_to_identifiers(columns, location),
    do: Enum.map(columns, &{:identifier, &1.table.name, {:unquoted, &1.column.name}, location})

  defp verify_star_select(columns, query) do
    case columns -- Enum.uniq(columns) do
      [] ->
        columns

      [{:identifier, table_name, {:unquoted, column_name}, _} | _] ->
        raise CompilationError,
          source_location: Query.source_location(query),
          message:
            "Selecting all from subquery `#{table_name}` is not supported" <>
              " because the column name `#{column_name}` is ambiguous."
    end
  end

  defp compile_titles(%Query{command: :union, from: {:union, {:subquery, lhs}, _}} = query) do
    %Query{query | column_titles: lhs.ast.column_titles}
  end

  defp compile_titles(%Query{command: :select, subquery?: true} = query) do
    column_titles =
      query.columns
      |> Enum.map(&column_title(&1, query.selected_tables))
      |> Enum.with_index(1)
      |> Enum.map(fn {title, index} -> title || "__ac_??#{index}" end)

    %Query{query | column_titles: column_titles}
  end

  defp compile_titles(%Query{command: :select, subquery?: false} = query) do
    column_titles =
      query.columns
      |> Enum.map(&column_title(&1, query.selected_tables))
      |> Enum.map(fn
        "__ac_??" <> _ -> ""
        title -> title || ""
      end)

    %Query{query | column_titles: column_titles}
  end

  defp compile_grouping_sets(%Query{group_by: []} = query) do
    group_by = query.grouping_sets |> List.flatten() |> Expression.unique()

    grouping_sets =
      Enum.map(query.grouping_sets, fn grouping_set ->
        Enum.map(grouping_set, fn column ->
          Enum.find_index(group_by, &Expression.equals?(&1, column))
        end)
      end)

    %Query{query | group_by: group_by, grouping_sets: grouping_sets}
  end

  defp compile_aliases(%Query{columns: [_ | _] = columns} = query) do
    verify_aliases(query)

    aliases = for {column, :as, name} <- columns, into: %{}, do: {name, column}

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

  defp resolve_alias(aliases, identifier = {:identifier, :unknown, {_quoted, column_name}, _loc}),
    do: Map.get(aliases, column_name, identifier)

  defp resolve_alias(_aliases, other), do: other

  defp column_title({_identifier, :as, alias}, _selected_tables), do: alias

  defp column_title({:function, {:cast, _}, [expression], _}, selected_tables),
    do: column_title(expression, selected_tables)

  defp column_title({:function, name, _, _}, _selected_tables), do: Function.readable_name(name)

  defp column_title({:distinct, identifier}, selected_tables), do: column_title(identifier, selected_tables)

  # If the table doesn't exist, we assume we have a dotted column name.
  defp column_title({:identifier, {:unquoted, table}, {:unquoted, column}, _}, selected_tables) do
    if find_table(selected_tables, {:unquoted, table}) == nil,
      do: "#{table}.#{column}",
      else: column
  end

  defp column_title({:identifier, _table, {_, column}, _}, _selected_tables), do: column
  defp column_title(:null, _selected_tables), do: nil
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

  defp compile_columns(%Query{command: :union, from: {:union, {:subquery, lhs}, _}} = query) do
    %Query{query | columns: Enum.map(lhs.ast.columns, &Expression.constant(&1.type, nil))}
  end

  defp compile_columns(%Query{command: :select} = query) do
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

  defp identifier_to_column({:function, name, args, location} = function, _columns_by_name, _query) do
    function
    |> verify_function_exists()
    |> Function.return_type()
    |> case do
      :unknown ->
        raise CompilationError,
          source_location: location,
          message: function_argument_error_message(function)

      type ->
        Expression.function(
          Function.canonical_name(name),
          args,
          type
        )
    end
    |> Expression.set_location(location)
  end

  defp identifier_to_column({:parameter, index}, _columns_by_name, query) do
    param_value = if query.parameters != nil, do: Enum.at(query.parameters, index - 1).value
    param_type = Query.parameter_type(query, index)
    if param_type == :unknown, do: parameter_error(query, index)
    Expression.constant(param_type, param_value)
  end

  defp identifier_to_column({:constant, type, value, location}, _columns_by_name, _query) do
    case parse_literal(type, value) do
      {:ok, value} ->
        Expression.constant(type, value) |> Expression.set_location(location)

      {:error, reason} ->
        raise CompilationError,
          source_location: location,
          message: invalid_literal_error_message(type, value, reason)
    end
  end

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

  defp identifier_to_column(:null, _columns_by_name, _query), do: Expression.null()

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

  defp verify_function_exists(function = {:function, name, _, location}) do
    unless Function.exists?(function) do
      case Function.deprecation_info(function) do
        {:error, error} when error in [:not_found, :unsafe_function] ->
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
        {:distinct, %Expression{} = expression} -> expression.type
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

  defp parse_literal(:time, value), do: Cloak.Time.parse_time(value)
  defp parse_literal(:date, value), do: Cloak.Time.parse_date(value)
  defp parse_literal(:datetime, value), do: Cloak.Time.parse_datetime(value)
  defp parse_literal(:interval, value), do: Timex.Duration.parse(value)
  defp parse_literal(_type, value), do: {:ok, value}

  defp invalid_literal_error_message(type, value, :invalid_format),
    do: "Invalid format for `#{type}` literal: '#{value}'."

  defp invalid_literal_error_message(type, value, :invalid_time),
    do: "Invalid time in `#{type}` literal: '#{value}'."

  defp invalid_literal_error_message(type, value, :invalid_date),
    do: "Invalid date in `#{type}` literal: '#{value}'."

  defp invalid_literal_error_message(type, value, reason), do: "Invalid `#{type}` literal: '#{value}' - #{reason}."

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
         %Expression{kind: :constant, type: :integer} = reference,
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

  defp compile_reference(%Expression{kind: :constant, type: _} = constant, _query, clause_name),
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

  @castable_types [:date, :text, :integer]

  defp perform_implicit_casts(query) do
    Lenses.query_expressions()
    |> Lens.filter(&Function.condition?/1)
    |> Lens.reject(&(&1.name == "not"))
    |> Lens.filter(&(Condition.verb(&1) in [:comparison, :in]))
    |> Lens.map(query, fn
      %Expression{kind: :function, name: "in", args: [subject | values]} = expression ->
        values = Enum.map(values, &perform_implicit_cast(&1, subject.type))
        %Expression{expression | args: [subject | values]}

      %Expression{kind: :function, args: [arg1, arg2]} = expression ->
        args =
          cond do
            arg1.kind == :constant and arg1.type in @castable_types -> [perform_implicit_cast(arg1, arg2.type), arg2]
            arg2.kind == :constant and arg2.type in @castable_types -> [arg1, perform_implicit_cast(arg2, arg1.type)]
            true -> [arg1, arg2]
          end

        %Expression{expression | args: args}
    end)
  end

  defp perform_implicit_cast(expression = %Expression{kind: :constant, type: :text}, type)
       when type in [:datetime, :time, :date] do
    case parse_time(expression.value, type) do
      {:ok, result} ->
        Expression.constant(type, result)

      {:error, reason} ->
        raise CompilationError,
          source_location: expression.source_location,
          message: invalid_literal_error_message(type, expression.value, reason)
    end
  end

  defp perform_implicit_cast(expression = %Expression{kind: :constant, type: :integer}, :real),
    do: %Expression{expression | type: :real}

  defp perform_implicit_cast(expression = %Expression{kind: :constant, type: :date}, :datetime) do
    casted_value = expression.value |> Timex.to_naive_datetime() |> Cloak.Time.max_precision()
    %Expression{expression | type: :datetime, value: casted_value}
  end

  defp perform_implicit_cast(expression, _type), do: expression

  defp parse_time(string, :date) when is_binary(string), do: Cloak.Time.parse_date(string)
  defp parse_time(string, :time) when is_binary(string), do: Cloak.Time.parse_time(string)

  defp parse_time(string, :datetime) when is_binary(string), do: Cloak.Time.parse_datetime(string)

  # -------------------------------------------------------------------
  # UID selection in a subquery
  # -------------------------------------------------------------------

  defp ensure_uid_selected(query) do
    case auto_select_uid_column(query) do
      nil ->
        query

      uid_column ->
        selected_expression = %Expression{uid_column | synthetic?: true}

        %Query{
          query
          | columns: query.columns ++ [selected_expression],
            column_titles: query.column_titles ++ [uid_column.name]
        }
    end
  end

  defp auto_select_uid_column(query) do
    cond do
      # top queries do not need an user id
      not query.subquery? ->
        nil

      # standard queries don't require user ids
      query.type == :standard ->
        nil

      # uid column is already explicitly selected
      Helpers.uid_column_selected?(query) ->
        nil

      # auto-selecting an user id here would change the behaviour of the query
      query.distinct? ->
        nil

      # no group by, no having and no aggregate -> select any uid column
      match?(%Query{group_by: [], having: nil}, query) && not Helpers.aggregates?(query) ->
        case Helpers.all_id_columns_from_tables(query) do
          [uid | _] -> uid
          [] -> nil
        end

      # uid column is in a group by -> select that uid
      (uid_column = Enum.find(query.group_by, & &1.user_id?)) != nil ->
        uid_column

      # we can't select a uid column
      true ->
        nil
    end
  end
end

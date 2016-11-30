defmodule Cloak.Aql.Compiler do
  @moduledoc "Makes the parsed SQL query ready for execution."

  alias Cloak.{DataSource, Features}
  alias Cloak.Aql.{Column, Comparison, FixAlign, Function, Parser, Query}
  alias Cloak.Aql.Parsers.Token
  alias Cloak.Query.DataDecoder

  defmodule CompilationError do
    @moduledoc false
    defexception message: "Error during compiling query"
  end


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Prepares the parsed SQL query for execution."
  @spec compile(DataSource.t, Parser.parsed_query, tuple, %{String.t => String.t}, Features.t) ::
    {:ok, Query.t} | {:error, String.t}
  def compile(data_source, parsed_query, parameters, views, features \\ Features.from_config()) do
    try do
      parsed_query
      |> to_prepped_query(data_source, features, parameters, views)
      |> compile_prepped_query()
    rescue
      e in CompilationError -> {:error, e.message}
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp to_prepped_query(parsed_query, data_source, features, parameters, views) do
    %Query{
      data_source: data_source,
      mode: query_mode(data_source.driver, parsed_query[:from]),
      features: features,
      parameters: parameters,
      views: views
    }
    |> Map.merge(parsed_query)
  end

  defp query_mode(Cloak.DataSource.DsProxy, {:subquery, %{type: :unparsed}}), do: :unparsed
  defp query_mode(Cloak.DataSource.DsProxy, from) do
    validate_dsproxy_from_for_parsed_query!(from)
    :parsed
  end
  defp query_mode(_other_data_source, _from), do: :parsed

  defp validate_dsproxy_from_for_parsed_query!(nil), do: :ok
  defp validate_dsproxy_from_for_parsed_query!({:join, join}) do
    validate_dsproxy_from_for_parsed_query!(join.lhs)
    validate_dsproxy_from_for_parsed_query!(join.rhs)
  end
  defp validate_dsproxy_from_for_parsed_query!({:subquery, _}) do
    raise CompilationError, message: "Joining subqueries is not supported for this data source."
  end
  defp validate_dsproxy_from_for_parsed_query!({_, table_name}) when is_binary(table_name), do: :ok

  # Due to the blackbox nature of the subquery, there are a whole lot
  # of validations we cannot do when using DS proxy. Conversely, there
  # are also built in cloak functionality that we cannot provide, like
  # regular, top level WHERE clauses.
  # We therefore perform custom DS proxy validations, in order to
  # keep the remaining validations clean, and free from having to
  # consider the DS Proxy case.
  defp compile_prepped_query(%Query{command: :select, mode: :unparsed} = query) do
    ds_proxy_validate_no_wildcard(query)
    ds_proxy_validate_no_where(query)
    query = query
    |> compile_columns()
    |> verify_columns()
    |> precompile_functions()
    |> censor_selected_uids()
    |> compile_order_by()
    |> partition_selected_columns()
    |> verify_having()
    |> calculate_db_columns()
    |> verify_limit()
    |> verify_offset()
    {:ok, query}
  end
  defp compile_prepped_query(%Query{command: :show} = query) do
    try do
      {:ok, query |> resolve_views() |> compile_subqueries() |> compile_tables()}
    rescue
      e in CompilationError -> {:error, e.message}
    end
  end
  defp compile_prepped_query(query) do
    try do
      query = query
      |> resolve_views()
      |> compile_subqueries()
      |> compile_tables()
      |> compile_columns()
      |> verify_columns()
      |> precompile_functions()
      |> censor_selected_uids()
      |> compile_order_by()
      |> verify_joins()
      |> cast_where_clauses()
      |> verify_where_clauses()
      |> align_ranges()
      |> partition_selected_columns()
      |> verify_having()
      |> partition_where_clauses()
      |> calculate_db_columns()
      |> verify_limit()
      |> verify_offset()
      {:ok, query}
    rescue
      e in CompilationError -> {:error, e.message}
    end
  end


  # -------------------------------------------------------------------
  # DS Proxy validators.
  # -------------------------------------------------------------------

  defp ds_proxy_validate_no_wildcard(%Query{command: :select, columns: :*}) do
    raise CompilationError, message: "Unfortunately wildcard selects are not supported together with subselects."
  end
  defp ds_proxy_validate_no_wildcard(_), do: :ok

  defp ds_proxy_validate_no_where(%Query{where: []}), do: :ok
  defp ds_proxy_validate_no_where(_) do
    raise CompilationError, message: "WHERE-clause in outer SELECT is not allowed in combination with a subquery."
  end


  # -------------------------------------------------------------------
  # Views
  # -------------------------------------------------------------------

  defp resolve_views(%Query{from: nil} = query), do: query
  defp resolve_views(query) do
    compiled = do_resolve_views(query.from, query)
    %Query{query | from: compiled}
  end

  defp do_resolve_views({:join, join}, query) do
    {:join, %{join |
      lhs: do_resolve_views(join.lhs, query),
      rhs: do_resolve_views(join.rhs, query)
    }}
  end
  defp do_resolve_views({_, name} = table_or_view, query) when is_binary(name) do
    case Map.fetch(query.views, name) do
      {:ok, view_sql} -> view_to_subquery(name, view_sql, query)
      :error -> table_or_view
    end
  end
  defp do_resolve_views(other, _query), do:
    other

  def view_to_subquery(view_name, view_sql, query) do
    case Cloak.Aql.Parser.parse(query.data_source, view_sql) do
      {:ok, parsed_view} -> {:subquery, %{type: :parsed, ast: parsed_view, alias: view_name}}
      {:error, error} -> raise CompilationError, message: "Error in the view `#{view_name}`: #{error}"
    end
  end


  # -------------------------------------------------------------------
  # Subqueries
  # -------------------------------------------------------------------

  defp compile_subqueries(%Query{from: nil} = query), do: query
  defp compile_subqueries(query) do
    compiled = do_compile_subqueries(query.from, query)
    %Query{query | from: compiled, info: query.info ++ gather_info(compiled)}
  end

  defp do_compile_subqueries({:join, join}, query) do
    {:join, %{join |
      lhs: do_compile_subqueries(join.lhs, query),
      rhs: do_compile_subqueries(join.rhs, query)
    }}
  end
  defp do_compile_subqueries({:subquery, subquery}, query) do
    {:subquery, %{subquery | ast: compiled_subquery(subquery.ast, subquery.alias, query)}}
  end
  defp do_compile_subqueries(identifier = {_, table}, _query) when is_binary(table), do:
    identifier

  defp gather_info({:join, %{lhs: lhs, rhs: rhs}}), do: gather_info(rhs) ++ gather_info(lhs)
  defp gather_info({:subquery, subquery}), do: subquery.ast.info
  defp gather_info(_), do: []

  defp compiled_subquery(parsed_subquery, alias, parent_query) do
    case compile(
      parent_query.data_source,
      Map.put(parsed_subquery, :subquery?, :true),
      parent_query.parameters,
      parent_query.views
    ) do
      {:ok, compiled_query} ->
        compiled_query
        |> validate_uid(alias)
        |> validate_offset(alias)
        |> align_limit()
        |> align_offset()
      {:error, error} -> raise CompilationError, message: error
    end
  end

  @minimum_subquery_limit 10
  defp align_limit(query = %{limit: nil}), do: query
  defp align_limit(query = %{limit: limit}) do
    aligned = limit |> FixAlign.align() |> round() |> max(@minimum_subquery_limit)
    if aligned != limit do
      %{query | limit: aligned}
      |> add_info_message("Limit adjusted from #{limit} to #{aligned}")
    else
      query
    end
  end

  defp align_offset(query = %{offset: 0}), do: query
  defp align_offset(query = %{limit: limit, offset: offset}) do
    aligned = round(offset / limit) * limit
    if aligned != offset do
      %{query | offset: aligned}
      |> add_info_message("Offset adjusted from #{offset} to #{aligned}")
    else
      query
    end
  end

  defp validate_uid(subquery, alias) do
    case Enum.find(subquery.db_columns, &(&1.user_id?)) do
      nil ->
        possible_uid_columns =
          all_id_columns_from_tables(subquery)
          |> Enum.map(&Column.display_name/1)
          |> case do
            [column] -> "the column #{column}"
            columns -> "one of the columns #{Enum.join(columns, ", ")}"
          end

        raise CompilationError, message:
          "Missing a user id column in the select list of subquery `#{alias}`. " <>
          "To fix this error, add #{possible_uid_columns} to the subquery select list."
      _ ->
        subquery
    end
  end

  defp validate_offset(%{offset: offset, limit: limit}, alias) when is_nil(limit) and offset > 0, do:
      raise CompilationError, message: "Subquery `#{alias}` has an OFFSET clause without a LIMIT clause."
  defp validate_offset(subquery, _), do: subquery


  # -------------------------------------------------------------------
  # Normal validators and compilers
  # -------------------------------------------------------------------

  defp compile_tables(%Query{from: nil} = query), do: query
  defp compile_tables(query) do
    normalized = normalize_from(query.from, query.data_source)
    %Query{query |
      from: normalized,
      selected_tables: selected_tables(normalized, query.data_source),
    }
  end

  defp normalize_from({:join, join = %{lhs: lhs, rhs: rhs}}, data_source) do
    {:join, %{join | lhs: normalize_from(lhs, data_source), rhs: normalize_from(rhs, data_source)}}
  end
  defp normalize_from(already_compiled_subquery = {:subquery, _}, _data_source), do: already_compiled_subquery
  defp normalize_from(table_identifier = {_, table_name}, data_source) do
    case table(data_source, table_identifier) do
      nil -> raise CompilationError, message: "Table `#{table_name}` doesn't exist."
      table -> table.name
    end
  end

  defp table(data_source, {:quoted, name}), do: DataSource.table(data_source, name)
  defp table(data_source, {:unquoted, name}) do
    data_source.tables
    |> Enum.find(fn({_id, table}) -> insensitive_equal?(table.name, name) end)
    |> case do
      {_id, table} -> table
      nil -> nil
    end
  end

  defp selected_tables({:join, join}, data_source) do
    selected_tables(join.lhs, data_source) ++ selected_tables(join.rhs, data_source)
  end
  defp selected_tables({:subquery, subquery}, _data_source) do
    user_id = Enum.find(subquery.ast.db_columns, &(&1.user_id?))
    columns = Enum.map(subquery.ast.db_columns, &{&1.alias || &1.name, &1.type})
    [%{
      name: subquery.alias,
      db_name: nil,
      columns: columns,
      user_id: user_id.alias || user_id.name
    }]
  end
  defp selected_tables(table_name, data_source) when is_binary(table_name) do
    case DataSource.table(data_source, table_name) do
      nil -> raise CompilationError, message: "Table `#{table_name}` doesn't exist."
      table -> [table]
    end
  end

  defp compile_aliases(%Query{columns: [_|_] = columns} = query) do
    verify_aliases(query)
    column_titles = Enum.map(columns, fn
      ({_column, :as, name}) -> name
      (column) -> column_title(column)
    end)
    aliases = for {column, :as, name} <- columns, into: %{}, do: {{:identifier, :unknown, {:unquoted, name}}, column}
    columns = Enum.map(columns, fn
      ({column, :as, _name}) -> column
      (column) -> column
    end)
    order_by = for {column, direction} <- query.order_by, do: {Map.get(aliases, column, column), direction}
    group_by = for identifier <- query.group_by, do: Map.get(aliases, identifier, identifier)
    %Query{query | column_titles: column_titles, columns: columns, group_by: group_by, order_by: order_by}
  end
  defp compile_aliases(query), do: query

  # Subqueries can produce column-names that are not actually in the table. Without understanding what
  # is being produced by the subquery (currently it is being treated as a blackbox), we cannot validate
  # the outer column selections
  defp verify_aliases(%Query{command: :select, mode: :unparsed}), do: :ok
  defp verify_aliases(query) do
    aliases = for {_column, :as, name} <- query.columns, do: name
    all_identifiers = aliases ++ all_column_identifiers(query)
    referenced_names = (for {{:identifier, _table, {_, name}}, _direction} <- query.order_by, do: name) ++
      query.group_by
    ambiguous_names = for name <- referenced_names, Enum.count(all_identifiers, &name == &1) > 1, do: name
    case ambiguous_names do
      [] -> :ok
      [name | _rest] -> raise CompilationError, message: "Usage of `#{name}` is ambiguous."
    end
  end

  defp invalid_individual_columns(%Query{command: :select, group_by: [_|_]} = query), do:
    Enum.filter(query.columns, &individual_column?(&1, query))
  defp invalid_individual_columns(%Query{command: :select} = query) do
    query.columns
    |> Enum.reject(&constant_column?/1)
    |> Enum.partition(&aggregated_column?(&1, query))
    |> case  do
      {[_|_] = _aggregates, [_|_] = individual_columns} -> individual_columns
      _ -> []
    end
  end

  defp aggregated_column?(column, query), do:
    Enum.member?(query.group_by, column) or
    (
      Function.function?(column) and
      (
        Function.aggregate_function?(column) or
        column |> Function.arguments() |> Enum.any?(&aggregated_column?(&1, query))
      )
    ) or
    (
      Column.db_function?(column) and
      (
        column.aggregate? or
        Enum.any?(column.db_function_args, &aggregated_column?(&1, query))
      )
    )

  defp constant_column?(column), do:
    Column.constant?(column) or
    (
      Function.function?(column) and
      column |> Function.arguments() |> Enum.all?(&constant_column?/1)
    )

  defp individual_column?(column, query), do:
    not constant_column?(column) and not aggregated_column?(column, query)

  defp compile_columns(query) do
    query
    |> expand_star_select()
    |> compile_buckets()
    |> compile_aliases()
    |> identifiers_to_columns()
  end

  defp compile_buckets(query) do
    {columns, messages} = Enum.reduce(query.columns, {[], []}, &compile_bucket/2)
    %{query | columns: Enum.reverse(columns), info: messages ++ query.info}
  end

  defp compile_bucket(column, {output_columns, messages}) do
    if Function.bucket?(column) do
      align_bucket(column, {output_columns, messages})
    else
      {[column | output_columns], messages}
    end
  end

  defp align_bucket(column, {output_columns, messages}) do
    if Function.bucket_size(column) <= 0 do
      raise CompilationError, message: "Bucket size #{Function.bucket_size(column)} must be > 0"
    end

    aligned = Function.update_bucket_size(column, &FixAlign.align/1)
    if aligned == column do
      {[column | output_columns], messages}
    else
      {
        [aligned | output_columns],
        ["Bucket size adjusted from #{Function.bucket_size(column)} to #{Function.bucket_size(aligned)}" | messages]
      }
    end
  end

  defp expand_star_select(%Query{columns: :*} = query) do
    columns = all_column_identifiers(query)
    column_names = for {:identifier, _table, {_, name}} <- columns, do: name
    %Query{query | columns: columns, column_titles: column_names}
  end
  defp expand_star_select(query), do: query

  defp filter_aggregators(columns), do:
    columns
    |> Enum.flat_map(&expand_arguments/1)
    |> Enum.filter(&Function.aggregate_function?/1)

  defp verify_columns(query) do
    verify_functions(query)
    verify_aggregated_columns(query)
    verify_group_by_functions(query)
    verify_function_arguments(query)
    query
  end

  defp verify_function_arguments(%Query{mode: :unparsed}), do: :ok
  defp verify_function_arguments(query) do
    query.columns
    |> Enum.flat_map(&expand_arguments/1)
    |> Enum.reject(&Function.well_typed?/1)
    |> case do
      [] -> :ok
      [function_call | _rest] ->
        raise CompilationError, message: function_argument_error_message(function_call)
    end
  end

  defp function_argument_error_message(function_call) do
    cond do
      Function.cast?(function_call) ->
        [cast_source] = actual_types(function_call)
        cast_target = Function.return_type(function_call)
        "Cannot cast value of type `#{cast_source}` to type `#{cast_target}`."
      many_overloads?(function_call) ->
        "Arguments of type (#{function_call |> actual_types() |> quoted_list()}) are incorrect"
          <> " for `#{Function.name(function_call)}`."
      true ->
        "Function `#{Function.name(function_call)}` requires arguments of type #{expected_types(function_call)}"
          <> ", but got (#{function_call |> actual_types() |> quoted_list()})."
    end
  end

  defp precompile_functions(%Query{} = query) do
    %Query{query |
      columns: precompile_functions(query.columns),
      group_by: precompile_functions(query.group_by),
      order_by: (for {column, direction} <- query.order_by, do: {precompile_function(column), direction}),
    }
  end
  defp precompile_functions(columns), do:
    Enum.map(columns, &precompile_function/1)

  defp precompile_function({:function, _function, _args} = function_spec) do
    case Function.compile_function(function_spec, &precompile_functions/1) do
      {:error, message} -> raise CompilationError, message: message
      compiled_function -> compiled_function
    end
  end
  defp precompile_function(column), do: column

  defp many_overloads?(function_call) do
    length(Function.argument_types(function_call)) > 4
  end

  defp expected_types(function_call), do:
    Function.argument_types(function_call)
    |> Enum.map(&quoted_list/1)
    |> Enum.map(&"(#{&1})")
    |> Enum.join(" or ")

  defp actual_types(function_call), do:
    Function.arguments(function_call) |> Enum.map(&Function.type/1)

  defp expand_arguments(column) do
    (column |> Function.arguments() |> Enum.flat_map(&expand_arguments/1)) ++ [column]
  end

  defp quoted_list(items), do:
    items |> Enum.map(&quoted_item/1) |> Enum.join(", ")

  defp quoted_item({:optional, type}), do: "[`#{type}`]"
  defp quoted_item({:many1, type}), do: "[`#{type}`]+"
  defp quoted_item({:or, types}), do: types |> Enum.map(&quoted_item/1) |> Enum.join(" | ")
  defp quoted_item(item), do: "`#{item}`"

  defp verify_aggregated_columns(query) do
    case invalid_individual_columns(query) do
      [] -> :ok
      [column | _rest] ->
        raise CompilationError, message: "#{aggregated_expression_display(column)} " <>
          "to appear in the `GROUP BY` clause or be used in an aggregate function."
    end
  end

  defp aggregated_expression_display({:function, _function, [arg]}), do:
    "Column #{quoted_item(arg.name)} needs"
  defp aggregated_expression_display({:function, _function, args}), do:
    "Columns (#{args |> Enum.map(&(&1.name)) |> quoted_list()}) need"
  defp aggregated_expression_display(%Column{db_function: fun, db_function_args: args}) when fun != nil do
    [column | _] = for %Column{constant?: false} = column <- args, do: column
    aggregated_expression_display(column)
  end
  defp aggregated_expression_display(%Column{table: table, name: name}), do:
    "Column `#{name}` from table `#{table.name}` needs"

  defp verify_group_by_functions(query) do
    query.group_by
    |> Enum.filter(&Function.aggregate_function?/1)
    |> case do
      [] -> :ok
      [function | _] -> raise CompilationError,
        message: "Aggregate function `#{Function.name(function)}` can not be used in the `GROUP BY` clause."
    end
  end

  defp verify_functions(query) do
    query.columns
    |> Enum.filter(&Function.function?/1)
    |> Enum.each(&check_function_validity(&1, query))
  end

  defp check_function_validity(function, query) do
    verify_function_exists(function)
    verify_function_supported_feature(function, query)
    verify_function_subquery_usage(function, query)
  end

  defp verify_function_exists(function) do
    unless Function.exists?(function) do
      raise CompilationError, message: "Unknown function `#{Function.name(function)}`."
    end
  end

  defp verify_function_supported_feature(function, query) do
    unless Function.valid_feature?(function, query) do
      raise CompilationError, message:
        "Function `#{Function.name(function)}` requires feature " <>
        "`#{Function.required_feature(function)}` to be enabled."
    end
  end

  defp verify_function_subquery_usage(_function, %Query{subquery?: false}), do: :ok
  defp verify_function_subquery_usage(function, %Query{subquery?: true}) do
    unless Function.allowed_in_subquery?(function) do
      raise CompilationError, message:
        "Function `#{Function.name(function)}` is not allowed in subqueries."
    end
  end

  defp all_column_identifiers(query) do
    for table <- query.selected_tables, {column_name, _type} <- table.columns do
      {:identifier, table.name, {:unquoted, column_name}}
    end
  end

  defp partition_selected_columns(%Query{subquery?: true} = query), do: query
  defp partition_selected_columns(%Query{group_by: groups = [_|_], columns: selected_columns} = query) do
    having_columns = Enum.flat_map(query.having, fn ({:comparison, column, _operator, target}) -> [column, target] end)
    aggregators = filter_aggregators(selected_columns ++ having_columns)
    %Query{query | property: groups |> Enum.uniq(), aggregators: aggregators |> Enum.uniq()}
  end
  defp partition_selected_columns(%Query{columns: selected_columns} = query) do
    case filter_aggregators(selected_columns) do
      [] ->
        %Query{query |
          property: selected_columns |> Enum.uniq(), aggregators: [{:function, "count", [:*]}], implicit_count: true
        }
      aggregators ->
        %Query{query | property: [], aggregators: aggregators |> Enum.uniq()}
    end
  end
  defp partition_selected_columns(query), do: query

  defp compile_order_by(%Query{order_by: []} = query), do: query
  defp compile_order_by(%Query{columns: columns, order_by: order_by_spec} = query) do
    invalid_fields = Enum.reject(order_by_spec, fn ({column, _direction}) -> Enum.member?(columns, column) end)
    case invalid_fields do
      [] ->
        order_list = for {column, direction} <- order_by_spec do
          index = columns |> Enum.find_index(&(&1 == column))
          {index, direction}
        end
        %Query{query | order_by: order_list}
      [{_column, _direction} | _rest] ->
        raise CompilationError, message: "Non-selected column specified in `ORDER BY` clause."
    end
  end

  defp partition_where_clauses(%Query{subquery?: true} = query) do
    case Enum.find(query.where, &requires_lcf_check?/1) do
      nil -> :ok
      condition ->
        raise CompilationError,
          message: "#{negative_condition_string(condition)} is not supported in a subquery."
    end
    case Enum.find(query.where, &Comparison.subject(&1) |> DataDecoder.needs_decoding?()) do
      nil -> :ok
      condition ->
        column = Comparison.subject(condition)
        raise CompilationError,
          message: "Column #{Column.display_name(column)} needs decoding and can not be used in a subquery."
    end
    query
  end
  defp partition_where_clauses(query) do
    # extract conditions requiring low-count filtering
    {require_lcf_checks, safe_clauses} = Enum.partition(query.where, &requires_lcf_check?/1)
    # split LCF conditions into positives and negatives
    {negative_lcf_checks, positive_lcf_checks} = Enum.partition(require_lcf_checks, &Comparison.negative?/1)
    # convert negative LCF conditions into checks for `IS NOT NULL`
    filter_null_lcf_columns = Enum.map(negative_lcf_checks, &{:not, {:is, Comparison.subject(&1), :null}})
    # forward normal conditions, positive LCF conditions and `IS NOT NULL` checks for negative LCF conditions to driver
    safe_clauses = Enum.uniq(safe_clauses ++ positive_lcf_checks ++ filter_null_lcf_columns)
    # extract conditions using encoded columns
    {encoded_column_clauses, safe_clauses} = Enum.partition(safe_clauses, &encoded_column_condition?/1)
    # extract columns needed in the cloak for extra filtering
    unsafe_filter_columns = Enum.map(require_lcf_checks ++ encoded_column_clauses, &Comparison.subject/1)

    %Query{query | where: safe_clauses, lcf_check_conditions: require_lcf_checks,
      unsafe_filter_columns: unsafe_filter_columns, encoded_where: encoded_column_clauses}
  end

  defp requires_lcf_check?({:not, {:is, _, :null}}), do: false
  defp requires_lcf_check?({:not, _other}), do: true
  defp requires_lcf_check?({:in, _column, _values}), do: true
  defp requires_lcf_check?(_other), do: false

  defp encoded_column_condition?(condition), do:
    Comparison.verb(condition) != :is and Comparison.subject(condition) |> DataDecoder.needs_decoding?()

  defp verify_joins(query) do
    join_conditions_scope_check(query.from)
    Enum.each(all_join_conditions(query.from), &verify_supported_join_condition/1)

    # Algorithm for finding improperly joined tables:
    #
    # 1. Create a DCG graph, where all uid columns are vertices.
    # 2. Add an edge for all where clauses shaped as `uid1 = uid2`
    # 3. Find the first pair (uid1, uid2) where there is no path from uid1 to uid2 in the graph.
    # 4. Report an error if something is found in the step 3

    column_key = fn(column) -> {column.name, column.table.db_name} end

    graph = :digraph.new([:private, :cyclic])
    try do
      # add uid columns as vertices
      uid_columns = Enum.map(query.selected_tables, &%Column{name: &1.user_id, table: &1})
      Enum.each(uid_columns, &:digraph.add_vertex(graph, column_key.(&1)))

      # add edges for all `uid1 = uid2` filters
      for {:comparison, column1, :=, column2} <- query.where ++ all_join_conditions(query.from),
          column1 != column2,
          column1.user_id?,
          column2.user_id?
      do
        :digraph.add_edge(graph, column_key.(column1), column_key.(column2))
        :digraph.add_edge(graph, column_key.(column2), column_key.(column1))
      end

      # Find first pair (uid1, uid2) which are not connected in the graph.
      uid_columns
      |> Stream.chunk(2, 1)
      |> Stream.filter(
            fn([uid1, uid2]) -> :digraph.get_path(graph, column_key.(uid1), column_key.(uid2)) == false end
          )
      |> Enum.take(1)
      |> case do
            [] ->
              # No such pair -> all tables are properly joined
              query

            [[column1, column2]] ->
              table1 = column1.table.name
              table2 = column2.table.name
              raise CompilationError,
                message:
                  "Missing where comparison for uid columns of tables `#{table1}` and `#{table2}`. " <>
                  "You can fix the error by adding `#{table1}.#{column1.name} = #{table2}.#{column2.name}` " <>
                  "condition to the `WHERE` clause."
          end
    after
      # digraph is powered by ets tables, so we need to make sure they are deleted once we don't need them
      :digraph.delete(graph)
    end
  end

  @spec all_join_conditions(Parser.from_clause) :: [Parser.where_clause]
  defp all_join_conditions({:join, join}) do
    join.conditions ++ all_join_conditions(join.lhs) ++ all_join_conditions(join.rhs)
  end
  defp all_join_conditions(_), do: []

  defp align_ranges(%Query{where: [_|_] = clauses} = query) do
    verify_ranges(query)

    ranges = inequalities_by_column(clauses)
    non_range_clauses = Enum.reject(clauses, &Enum.member?(Map.keys(ranges), Comparison.subject(&1)))

    Enum.reduce(ranges, %{query | where: non_range_clauses}, &add_aligned_range/2)
  end
  defp align_ranges(query), do: query

  defp add_aligned_range({column, conditions}, query) do
    {left, right} =
      conditions
      |> Enum.map(&Comparison.value/1)
      |> Enum.sort(&lt_eq/2)
      |> List.to_tuple()
      |> FixAlign.align_interval()

    if implement_range?({left, right}, conditions) do
      %{query | where: conditions ++ query.where}
    else
      query
      |> add_where_clause({:comparison, column, :<, Column.constant(column.type, right)})
      |> add_where_clause({:comparison, column, :>=, Column.constant(column.type, left)})
      |> add_info_message(
        "The range for column `#{column.name}` has been adjusted to #{left} <= `#{column.name}` < #{right}"
      )
    end
  end

  defp implement_range?({left, right}, conditions) do
    [{_, _, left_operator, left_column}, {_, _, right_operator, right_column}] =
      Enum.sort_by(conditions, &Comparison.value/1)

    left_operator == :>= && left_column.value == left && right_operator == :< && right_column.value == right
  end

  defp add_where_clause(query, clause), do: %{query | where: [clause | query.where]}

  defp verify_ranges(%Query{where: clauses}) do
    clauses
    |> inequalities_by_column()
    |> Enum.reject(fn({_, comparisons}) -> valid_range?(comparisons) end)
    |> case do
      [{column, _} | _] -> raise CompilationError, message: "Column `#{column.name}` must be limited to a finite range."
      _ -> :ok
    end
  end

  defp valid_range?(comparisons) do
    case Enum.sort_by(comparisons, &Comparison.direction/1, &Kernel.>/2) do
      [cmp1, cmp2] ->
        Comparison.direction(cmp1) != Comparison.direction(cmp2) &&
          lt_eq(Comparison.value(cmp1), Comparison.value(cmp2))
      _ -> false
    end
  end

  defp lt_eq(x = %NaiveDateTime{}, y = %NaiveDateTime{}), do: Timex.diff(x, y) <= 0
  defp lt_eq(x = %Date{}, y = %Date{}), do: Timex.diff(x, y) <= 0
  defp lt_eq(x = %Time{}, y = %Time{}), do: Cloak.Time.time_to_seconds(x) <= Cloak.Time.time_to_seconds(y)
  defp lt_eq(x, y), do: x <= y

  @aligned_types ~w(integer real datetime date time)a
  defp inequalities_by_column(where_clauses) do
    where_clauses
    |> Enum.filter(&Comparison.inequality?/1)
    |> Enum.group_by(&Comparison.subject/1)
    |> Enum.filter(fn({column, _}) -> Enum.member?(@aligned_types, column.type) end)
    |> Enum.map(&discard_redundant_inequalities/1)
    |> Enum.into(%{})
  end

  defp discard_redundant_inequalities({column, inequalities}) do
    case {bottom, top} = Enum.partition(inequalities, &(Comparison.direction(&1) == :>)) do
      {[], []} -> {column, []}
      {_, []} -> {column, [Enum.max_by(bottom, &Comparison.value/1)]}
      {[], _} -> {column, [Enum.min_by(top, &Comparison.value/1)]}
      {_, _} -> {column, [Enum.max_by(bottom, &Comparison.value/1), Enum.min_by(top, &Comparison.value/1)]}
    end
  end

  defp cast_where_clauses(%Query{where: [_|_] = clauses} = query) do
    %Query{query | where: Enum.map(clauses, &cast_where_clause/1)}
  end
  defp cast_where_clauses(query), do: query

  defp cast_where_clause(clause) do
    column = Comparison.subject(clause)
    do_cast_where_clause(clause, column.type)
  end

  @castable_conditions [:datetime, :time, :date]

  defp do_cast_where_clause({:not, subclause}, type), do:
    {:not, do_cast_where_clause(subclause, type)}
  defp do_cast_where_clause({:comparison, identifier, comparator, rhs}, type) when type in @castable_conditions do
    if Column.constant?(rhs) do
      {:comparison, identifier, comparator, parse_time(rhs, type)}
    else
      {:comparison, identifier, comparator, rhs}
    end
  end
  defp do_cast_where_clause({:in, column, values}, type) when type in @castable_conditions, do:
    {:in, column, Enum.map(values, &parse_time(&1, type))}
  defp do_cast_where_clause(clause, _), do: clause

  defp parse_time(column = %Column{constant?: true, value: string}, type) do
    case do_parse_time(column, type) do
      {:ok, result} -> Column.constant(type, result)
      _ -> raise CompilationError, message: "Cannot cast `#{string}` to #{type}."
    end
  end

  defp do_parse_time(%Column{type: :text, value: string}, :date), do:
    Cloak.Time.parse_date(string)
  defp do_parse_time(%Column{type: :text, value: string}, :time), do:
    Cloak.Time.parse_time(string)
  defp do_parse_time(%Column{type: :text, value: string}, :datetime), do:
    Cloak.Time.parse_datetime(string)
  defp do_parse_time(_, _), do: {:error, :invalid_cast}

  defp map_terminal_elements(query, mapper_fun) do
    %Query{query |
      columns: Enum.map(query.columns, &map_terminal_element(&1, mapper_fun)),
      group_by: Enum.map(query.group_by, &map_terminal_element(&1, mapper_fun)),
      where: Enum.map(query.where, &map_where_clause(&1, mapper_fun)),
      lcf_check_conditions: Enum.map(query.lcf_check_conditions, &map_where_clause(&1, mapper_fun)),
      encoded_where: Enum.map(query.encoded_where, &map_where_clause(&1, mapper_fun)),
      order_by: Enum.map(query.order_by, &map_order_by(&1, mapper_fun)),
      having: Enum.map(query.having, &map_where_clause(&1, mapper_fun)),
      db_columns: Enum.map(query.db_columns, &map_terminal_element(&1, mapper_fun)),
      property: Enum.map(query.property, &map_terminal_element(&1, mapper_fun)),
      aggregators: Enum.map(query.aggregators, &map_terminal_element(&1, mapper_fun)),
      from: map_join_conditions_columns(query.from, mapper_fun)
    }
  end

  defp map_join_conditions_columns({:join, join}, mapper_fun) do
    {:join, %{join |
      lhs: map_join_conditions_columns(join.lhs, mapper_fun),
      rhs: map_join_conditions_columns(join.rhs, mapper_fun),
      conditions: Enum.map(join.conditions, &map_where_clause(&1, mapper_fun))
    }}
  end
  defp map_join_conditions_columns({:subquery, _} = subquery, _mapper_fun), do: subquery
  defp map_join_conditions_columns(raw_table_name, _mapper_fun) when is_binary(raw_table_name),
    do: raw_table_name

  defp map_where_clause({:comparison, lhs, comparator, rhs}, mapper_fun) do
    {
      :comparison,
      map_terminal_element(lhs, mapper_fun),
      comparator,
      map_terminal_element(rhs, mapper_fun)
    }
  end
  defp map_where_clause({:not, subclause}, mapper_fun) do
    {:not, map_where_clause(subclause, mapper_fun)}
  end
  Enum.each([:in, :like, :ilike, :is], fn(keyword) ->
    defp map_where_clause({unquote(keyword), lhs, rhs}, mapper_fun) do
      {unquote(keyword), map_terminal_element(lhs, mapper_fun), map_terminal_element(rhs, mapper_fun)}
    end
  end)

  defp map_order_by({identifier, direction}, mapper_fun),
    do: {map_terminal_element(identifier, mapper_fun), direction}

  defp map_terminal_element(%Token{} = token, mapper_fun), do: mapper_fun.(token)
  defp map_terminal_element(%Column{} = column, mapper_fun), do: mapper_fun.(column)
  defp map_terminal_element({:identifier, _, _} = identifier, mapper_fun), do: mapper_fun.(identifier)
  defp map_terminal_element({:function, "count", :*} = function, _converter_fun), do: function
  defp map_terminal_element({:function, "count_noise", :*} = function, _converter_fun), do: function
  defp map_terminal_element({:function, function, identifier}, converter_fun),
    do: converter_fun.({:function, function, map_terminal_element(identifier, converter_fun)})
  defp map_terminal_element({:distinct, identifier}, converter_fun),
    do: converter_fun.({:distinct, map_terminal_element(identifier, converter_fun)})
  defp map_terminal_element(elements, mapper_fun) when is_list(elements),
    do: Enum.map(elements, &map_terminal_element(&1, mapper_fun))
  defp map_terminal_element(constant, mapper_fun), do: mapper_fun.(constant)

  defp identifiers_to_columns(query) do
    columns_by_name =
      for table <- query.selected_tables, {column, type} <- table.columns do
        %Column{table: table, name: column, type: type, user_id?: table.user_id == column}
      end
      |> Enum.group_by(&(&1.name))

    query
    |> map_terminal_elements(&normalize_table_name(&1, query.data_source))
    |> map_terminal_elements(&identifier_to_column(&1, columns_by_name, query))
  end

  defp normalize_table_name({:identifier, table_identifier = {_, name}, column}, data_source) do
    case table(data_source, table_identifier) do
      nil -> {:identifier, name, column}
      table -> {:identifier, table.name, column}
    end
  end
  defp normalize_table_name(x, _), do: x

  defp identifier_to_column({:identifier, :unknown, {_, column_name}}, _columns_by_name, %Query{mode: :unparsed}),
    do: %Column{name: column_name, table: :unknown}
  defp identifier_to_column({:identifier, :unknown, identifier = {_, column_name}}, columns_by_name, _query) do
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
  defp identifier_to_column({:identifier, table, identifier = {_, column_name}}, columns_by_name, query) do
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
  defp identifier_to_column({:function, name, args} = function_spec, _columns_by_name, %Query{subquery?: true} = query) do
    check_function_validity(function_spec, query)
    case Function.return_type(function_spec) do
      nil -> raise CompilationError, message: function_argument_error_message(function_spec)
      type -> Column.db_function(name, args, type, Function.aggregate_function?(function_spec))
    end
  end
  defp identifier_to_column({:parameter, index}, _columns_by_name, query) do
    if index > length(query.parameters) do
      message =
        "The query references the `$#{index}` parameter, " <>
        "but only #{length(query.parameters)} parameters are passed."
      raise CompilationError, message: message
    end

    param_value = Enum.at(query.parameters, index - 1)
    Column.constant(data_type(param_value, index), param_value)
  end
  defp identifier_to_column({:constant, type, value}, _columns_by_name, _query), do:
    Column.constant(type, value)
  defp identifier_to_column(other, _columns_by_name, _query), do: other

  defp data_type(value, _index) when is_boolean(value), do: :boolean
  defp data_type(value, _index) when is_integer(value), do: :integer
  defp data_type(value, _index) when is_float(value), do: :real
  defp data_type(value, _index) when is_binary(value), do: :text
  defp data_type(_value, index), do:
    raise CompilationError, message: "Invalid value for the parameter `$#{index}`"

  defp get_columns(columns_by_name, {:unquoted, name}) do
    columns_by_name
    |> Enum.find(fn({key, _}) -> insensitive_equal?(name, key) end)
    |> case do
      nil -> nil
      {_, columns} -> columns
    end
  end
  defp get_columns(columns_by_name, {:quoted, name}), do: Map.get(columns_by_name, name)

  defp insensitive_equal?(s1, s2), do: String.downcase(s1) == String.downcase(s2)

  def column_title(function = {:function, _, _}), do: Function.name(function)
  def column_title({:distinct, identifier}), do: column_title(identifier)
  def column_title({:identifier, _table, {_, column}}), do: column
  def column_title({:constant, _, _}), do: ""

  defp censor_selected_uids(%Query{command: :select, subquery?: false} = query) do
    columns = for column <- query.columns, do:
      if is_uid_column?(column), do: Column.constant(:text, :*), else: column
    %Query{query | columns: columns}
  end
  defp censor_selected_uids(query), do: query

  defp is_uid_column?(column) do
    if Function.aggregate_function?(column) do
      false
    else
      column |> extract_columns() |> Enum.any?(& &1 != nil and &1.user_id?)
    end
  end

  defp extract_columns(%Column{} = column), do: [column]
  defp extract_columns({:function, "count", [:*]}), do: [nil]
  defp extract_columns({:function, "count_noise", [:*]}), do: [nil]
  defp extract_columns({:function, _function, arguments}), do: Enum.flat_map(arguments, &extract_columns/1)
  defp extract_columns({:distinct, expression}), do: extract_columns(expression)
  defp extract_columns({:comparison, column, _operator, target}), do: extract_columns(column) ++ extract_columns(target)
  defp extract_columns([columns]), do: Enum.flat_map(columns, &extract_columns(&1))

  defp add_info_message(query, info_message), do: %Query{query | info: [info_message | query.info]}

  defp calculate_db_columns(query) do
    query = %Query{query |
      db_columns:
        query
        |> select_expressions()
        |> Enum.uniq_by(&db_column_name/1)
    }

    map_terminal_elements(query, &set_column_db_row_position(&1, query))
  end

  defp select_expressions(%Query{command: :select, subquery?: false} = query) do
    # top-level query -> we,re fetching only columns, while other expressions (e.g. function calls)
    # will be resolved in the post-processing phase
    [id_column(query) | query.columns ++ query.group_by ++ query.unsafe_filter_columns ++ query.having]
    |> Enum.flat_map(&extract_columns/1)
    |> Enum.reject(& &1 == nil or &1.constant?)
  end
  defp select_expressions(%Query{command: :select, subquery?: true} = query) do
    # currently we don't support functions in subqueries
    true = Enum.all?(query.columns, &match?(%Column{}, &1))
    Enum.zip(query.column_titles, query.columns)
    |> Enum.map(fn({column_alias, column}) -> %Column{column | alias: column_alias} end)
  end

  defp id_column(query) do
    all_id_columns = all_id_columns_from_tables(query) |> Enum.map(&cast_unknown_id/1)
    if any_outer_join?(query.from),
      do: Column.db_function("coalesce", all_id_columns),
      else: hd(all_id_columns)
  end

  # We can't directly select a field with an unknown type, so convert it to binary
  # This is needed in the case of using the ODBC driver with a GUID user id,
  # as the GUID type is not supported by the Erlang ODBC library
  def cast_unknown_id(%Column{type: :unknown} = column), do: Column.db_function({:cast, :varbinary}, [column])
  def cast_unknown_id(column), do: column

  defp all_id_columns_from_tables(%Query{command: :select, mode: :unparsed}) do
    # We don't know the name of the user_id column for an unsafe query, so we're generating
    # a fake one instead.
    [%Column{table: :unknown, name: "__aircloak_user_id__", user_id?: true}]
  end
  defp all_id_columns_from_tables(%Query{command: :select, selected_tables: tables}) do
    Enum.map(tables, fn(table) ->
      user_id = table.user_id
      {_, type} = Enum.find(table.columns, fn ({name, _type}) -> insensitive_equal?(user_id, name) end)
      %Column{table: table, name: user_id, type: type, user_id?: true}
    end)
  end

  defp any_outer_join?(table) when is_binary(table), do: false
  defp any_outer_join?({:subquery, _}), do: false
  defp any_outer_join?({:join, %{type: type}})
    when type in [:full_outer_join, :left_outer_join, :right_outer_join],
    do: true
  defp any_outer_join?({:join, join}),
    do: any_outer_join?(join.lhs) || any_outer_join?(join.rhs)

  defp set_column_db_row_position(%Column{user_id?: true} = column, _query) do
    # the user-id columns will collectively all be available in the first position
    %Column{column | db_row_position: 0}
  end
  defp set_column_db_row_position(%Column{} = column, %Query{db_columns: db_columns}) do
    case Enum.find_index(db_columns, &(db_column_name(&1) == db_column_name(column))) do
      # It's not actually a selected column, so ignore for the purpose of positioning
      nil -> column
      position -> %Column{column | db_row_position: position}
    end
  end
  defp set_column_db_row_position(other, _query), do: other

  defp db_column_name(%Column{table: :unknown} = column), do: (column.name || column.alias)
  defp db_column_name(column), do: "#{column.table.db_name}.#{column.name}"

  defp join_conditions_scope_check(from) do
    do_join_conditions_scope_check(from, [])
  end

  defp do_join_conditions_scope_check({:join, join}, selected_tables) do
    selected_tables = do_join_conditions_scope_check(join.lhs, selected_tables)
    selected_tables = do_join_conditions_scope_check(join.rhs, selected_tables)
    mapper_fun = fn
      (%Cloak.Aql.Column{table: %{name: table_name}, name: column_name}) ->
        scope_check(selected_tables, table_name, column_name)
      ({:identifier, table_name, {_, column_name}}) -> scope_check(selected_tables, table_name, column_name)
      (_) -> :ok
    end
    Enum.each(join.conditions, &map_where_clause(&1, mapper_fun))
    Enum.each(join.conditions, &verify_where_clause/1)
    selected_tables
  end
  defp do_join_conditions_scope_check({:subquery, subquery}, selected_tables),
    do: [subquery.alias | selected_tables]
  defp do_join_conditions_scope_check(table_name, selected_tables) when is_binary(table_name),
    do: [table_name | selected_tables]

  defp scope_check(tables_in_scope, table_name, column_name) do
    case Enum.member?(tables_in_scope, table_name) do
      true -> :ok
      _ -> raise CompilationError, message: "Column `#{column_name}` of table `#{table_name}` is used out of scope."
    end
  end

  defp verify_supported_join_condition(join_condition) do
    if requires_lcf_check?(join_condition), do:
      raise CompilationError, message: "#{negative_condition_string(join_condition)} not supported in joins."
  end

  defp negative_condition_string({:not, {:like, _, _}}), do: "NOT LIKE"
  defp negative_condition_string({:not, {:ilike, _, _}}), do: "NOT ILIKE"
  defp negative_condition_string({:not, {:comparison, _, :=, _}}), do: "<>"
  defp negative_condition_string({:not, {:in, _, _}}), do: "NOT IN"
  defp negative_condition_string({:in, _, _}), do: "IN"

  defp verify_limit(%Query{command: :select, limit: amount}) when amount <= 0, do:
    raise CompilationError, message: "`LIMIT` clause expects a positive value."
  defp verify_limit(%Query{command: :select, order_by: [], limit: amount}) when amount != nil, do:
    raise CompilationError, message: "Using the `LIMIT` clause requires the `ORDER BY` clause to be specified."
  defp verify_limit(query), do: query

  defp verify_offset(%Query{command: :select, offset: amount}) when amount < 0, do:
    raise CompilationError, message: "`OFFSET` clause expects a non-negative value."
  defp verify_offset(%Query{command: :select, order_by: [], offset: amount}) when amount > 0, do:
    raise CompilationError, message: "Using the `OFFSET` clause requires the `ORDER BY` clause to be specified."
  defp verify_offset(query), do: query

  defp verify_having(%Query{command: :select, group_by: [], having: [_|_]}), do:
    raise CompilationError, message: "Using the `HAVING` clause requires the `GROUP BY` clause to be specified."
  defp verify_having(%Query{command: :select, having: [_|_]} = query) do
    for {:comparison, column, _operator, target} <- query.having, do:
      for term <- [column, target], do:
        if individual_column?(term, query), do:
          raise CompilationError,
            message: "`HAVING` clause can not be applied over column #{Column.display_name(term)}."
    query
  end
  defp verify_having(query), do: query

  defp verify_where_clauses(%Query{where: clauses = [_|_]} = query) do
    Enum.each(clauses, &verify_where_clause/1)
    query
  end
  defp verify_where_clauses(query), do: query

  defp verify_where_clause({:comparison, column_a, comparator, column_b}) do
    verify_where_clause_types(column_a, column_b)
    check_for_string_inequalities(comparator, column_b)
  end
  defp verify_where_clause({:like, column, _}) do
    if column.type != :text do
      raise CompilationError,
        message: "Column #{Column.display_name(column)} of type `#{column.type}` cannot be used in a LIKE expression."
    end
  end
  defp verify_where_clause({:not, clause}), do: verify_where_clause(clause)
  defp verify_where_clause(_), do: :ok


  defp verify_where_clause_types(column_a, column_b) do
    if not Column.constant?(column_a) and not Column.constant?(column_b) and column_a.type != column_b.type do
      raise CompilationError, message: "Column #{Column.display_name(column_a)} of type `#{column_a.type}` and "
        <> "column #{Column.display_name(column_b)} of type `#{column_b.type}` cannot be compared."
    end
  end

  defp check_for_string_inequalities(comparator, %Column{type: :text}) when comparator in [:>, :>=, :<, :<=], do:
    raise CompilationError, message: "Inequalities on string values are currently not supported."
  defp check_for_string_inequalities(_, _), do: :ok
end

defmodule Air.PsqlServer.Protocol.QueryExecution do
  @moduledoc false

  # Query execution part of PostgreSQL protocol. Handles simple and bound queries, as well as preparing
  # responses to clients.

  import Air.PsqlServer.Protocol.Helpers
  alias Air.PsqlServer.Protocol.Messages

  def handle_query_message(state, query), do:
    state
    |> add_action({:run_query, query, [], 0})
    |> next_state(:running_query)

  def handle_parse_message(state, prepared_statement) do
    prepared_statement = Map.merge(
      prepared_statement,
      %{params: nil, parsed_param_types: [], result_codes: nil, columns: nil}
    )

    state
    |> put_in([:prepared_statements, prepared_statement.name], prepared_statement)
    |> send_to_client(:parse_complete)
    |> transition_after_message(:ready)
  end

  def handle_bind_message(state, bind_data) do
    prepared_statement = Map.fetch!(state.prepared_statements, bind_data.name)

    param_types =
      case prepared_statement.param_types do
        [_|_] -> prepared_statement.param_types
        [] -> prepared_statement.parsed_param_types
      end

    params = Messages.convert_params(bind_data.params, bind_data.format_codes, param_types)

    state
    |> put_in([:prepared_statements, bind_data.name],
        %{prepared_statement | params: params, result_codes: bind_data.result_codes})
    |> send_to_client(:bind_complete)
    |> transition_after_message(:ready)
  end

  def handle_describe_message(state, describe_data) do
    prepared_statement = Map.fetch!(state.prepared_statements, describe_data.name)

    state
    |> add_action({:describe_statement, prepared_statement.query, params_with_types(prepared_statement)})
    |> next_state({:describing_statement, describe_data.name})
  end

  def handle_execute_message(state, execute_data) do
    prepared_statement = Map.fetch!(state.prepared_statements, execute_data.name)

    state
    |> add_action({:run_query, prepared_statement.query, params_with_types(prepared_statement),
      execute_data.max_rows})
    |> next_state({:running_prepared_statement, execute_data.name})
  end

  def handle_sync_message(state, _), do:
    state
    |> send_to_client(:ready_for_query)
    |> transition_after_message(:ready)

  def handle_flush_message(state, _), do:
    transition_after_message(state, :ready)

  def handle_close_message(state, close_data), do:
    state
    |> update_in([:prepared_statements], &Map.delete(&1, close_data.name))
    |> send_to_client(:close_complete)
    |> transition_after_message(:ready)

  def send_query_result(%{name: {:running_prepared_statement, name}} = state, result) do
    statement = Map.fetch!(state.prepared_statements, name)

    state
    |> send_rows(result.rows, statement.columns, statement.result_codes)
    |> send_to_client(:command_complete, ["SELECT #{length(result.rows)}"])
    |> send_to_client(:ready_for_query)
    |> transition_after_message(:syncing)
  end
  def send_query_result(state, result), do:
    state
    |> send_result(result)
    |> send_to_client(:ready_for_query)
    |> transition_after_message(:ready)

  def send_describe_result(state, %{error: error}), do:
    state
    |> send_to_client(:syntax_error_message, [error])
    |> transition_after_message(:ready)
  def send_describe_result(%{name: {:describing_statement, name}} = state, description) do
    prepared_statement = Map.fetch!(state.prepared_statements, name)

    result_codes = prepared_statement.result_codes || [:text]
    state
    |> put_in([:prepared_statements, name, :parsed_param_types], description.param_types)
    |> put_in([:prepared_statements, name, :columns], description.columns)
    |> send_parameter_descriptions(prepared_statement, description.param_types)
    |> send_to_client(:row_description, [description.columns, result_codes])
    |> transition_after_message(:ready)
  end

  defp send_parameter_descriptions(state, %{params: nil}, param_types), do:
    # parameters are not bound -> send parameter descriptions
    send_to_client(state, :parameter_description, [param_types])
  defp send_parameter_descriptions(state, _, _), do:
    # parameters are already bound -> client is not expecting parameter descriptions
    state

  defp send_result(state, nil), do:
    send_to_client(state, :command_complete, [""])
  defp send_result(state, %{rows: rows, columns: columns}), do:
    state
    |> send_to_client(:row_description, [columns, [:text]])
    |> send_rows(rows, columns, [:text])
    |> send_to_client(:command_complete, ["SELECT #{length(rows)}"])
  defp send_result(state, %{error: error}), do:
    send_to_client(state, :syntax_error_message, [error])

  defp send_rows(state, rows, columns, formats), do:
    Enum.reduce(rows, state, &send_to_client(&2, :data_row, [&1, column_types(columns), formats]))

  defp column_types(nil), do: Stream.cycle([:text])
  defp column_types(columns), do: Enum.map(columns, &(&1.type))

  defp params_with_types(%{params: nil}), do:
    nil
  defp params_with_types(prepared_statement) do
    param_types =
      cond do
        match?([_|_], prepared_statement.param_types) -> prepared_statement.param_types
        match?([_|_], prepared_statement.parsed_param_types) -> prepared_statement.parsed_param_types
        true -> Stream.cycle([:unknown])
      end

    Enum.zip(param_types, prepared_statement.params)
  end
end

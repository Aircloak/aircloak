defmodule Air.PsqlServer.Protocol.QueryExecution do
  @moduledoc """
  Handles the querying part of PostgreSQL protocol.

  For more details see [here](https://www.postgresql.org/docs/9.6/static/protocol-flow.html#AEN112852) and
  [here](https://www.postgresql.org/docs/9.6/static/protocol-flow.html#PROTOCOL-FLOW-EXT-QUERY).
  """

  alias Air.PsqlServer.Protocol
  alias Air.PsqlServer.Protocol.Messages

  @behaviour Protocol


  #-----------------------------------------------------------------------------------------------------------
  # Protocol callbacks
  #-----------------------------------------------------------------------------------------------------------

  @doc false
  def handle_client_message(protocol, :query, query), do:
    protocol
    |> Protocol.add_action({:run_query, query, [], 0})
    |> Protocol.next_state(:ready)
  def handle_client_message(protocol, :parse, prepared_statement) do
    prepared_statement = Map.merge(
      prepared_statement,
      %{params: nil, parsed_param_types: [], result_codes: nil, columns: nil}
    )

    protocol
    |> put_in([:prepared_statements, prepared_statement.name], prepared_statement)
    |> Protocol.send_to_client(:parse_complete)
    |> Protocol.await_and_decode_client_message(:ready)
  end
  def handle_client_message(protocol, :bind, bind_data) do
    prepared_statement = Map.fetch!(protocol.prepared_statements, bind_data.name)

    param_types =
      case prepared_statement.param_types do
        [_|_] -> prepared_statement.param_types
        [] -> prepared_statement.parsed_param_types
      end

    params = Messages.convert_params(bind_data.params, bind_data.format_codes, param_types)

    protocol
    |> put_in([:prepared_statements, bind_data.name],
        %{prepared_statement | params: params, result_codes: bind_data.result_codes})
    |> Protocol.send_to_client(:bind_complete)
    |> Protocol.await_and_decode_client_message(:ready)
  end
  def handle_client_message(protocol, :describe, describe_data) do
    prepared_statement = Map.fetch!(protocol.prepared_statements, describe_data.name)

    %{protocol | describing_statement: describe_data.name}
    |> Protocol.add_action({:describe_statement, prepared_statement.query, params_with_types(prepared_statement)})
    |> Protocol.next_state(:ready)
  end
  def handle_client_message(protocol, :execute, execute_data) do
    prepared_statement = Map.fetch!(protocol.prepared_statements, execute_data.name)

    %{protocol | running_prepared_statement: execute_data.name}
    |> Protocol.add_action({:run_query, prepared_statement.query, params_with_types(prepared_statement),
      execute_data.max_rows})
    |> Protocol.next_state(:ready)
  end
  def handle_client_message(protocol, :sync, _), do:
    protocol
    |> Protocol.send_to_client(:ready_for_query)
    |> Protocol.await_and_decode_client_message(:ready)
  def handle_client_message(protocol, :flush, _), do:
    Protocol.await_and_decode_client_message(protocol, :ready)
  def handle_client_message(protocol, :close, close_data), do:
    protocol
    |> update_in([:prepared_statements], &Map.delete(&1, close_data.name))
    |> Protocol.send_to_client(:close_complete)
    |> Protocol.await_and_decode_client_message(:ready)

  @doc false
  def handle_event(%{running_prepared_statement: name} = protocol, {:send_query_result, result}) when name != nil do
    statement = Map.fetch!(protocol.prepared_statements, name)

    protocol
    |> send_rows(result.rows, statement.columns, statement.result_codes)
    |> Protocol.send_to_client({:command_complete, "SELECT #{length(result.rows)}"})
    |> Protocol.send_to_client(:ready_for_query)
    |> Protocol.syncing()
    |> Protocol.await_and_decode_client_message(:ready)
  end
  def handle_event(protocol, {:send_query_result, result}), do:
    protocol
    |> send_result(result)
    |> Protocol.send_to_client(:ready_for_query)
    |> Protocol.await_and_decode_client_message(:ready)
  def handle_event(protocol, {:describe_result, %{error: error}}), do:
    protocol
    |> Protocol.send_to_client({:syntax_error, error})
    |> Protocol.await_and_decode_client_message(:ready)
  def handle_event(%{describing_statement: name} = protocol, {:describe_result, description}) do
    prepared_statement = Map.fetch!(protocol.prepared_statements, name)

    result_codes = prepared_statement.result_codes || [:text]
    protocol
    |> put_in([:prepared_statements, name, :parsed_param_types], description.param_types)
    |> put_in([:prepared_statements, name, :columns], description.columns)
    |> send_parameter_descriptions(prepared_statement, description.param_types)
    |> Protocol.send_to_client({:row_description, description.columns, result_codes})
    |> Protocol.await_and_decode_client_message(:ready)
  end


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp send_parameter_descriptions(protocol, %{params: nil}, param_types), do:
    # parameters are not bound -> send parameter descriptions
    Protocol.send_to_client(protocol, {:parameter_description, param_types})
  defp send_parameter_descriptions(protocol, _, _), do:
    # parameters are already bound -> client is not expecting parameter descriptions
    protocol

  defp send_result(protocol, nil), do:
    Protocol.send_to_client(protocol, {:command_complete, ""})
  defp send_result(protocol, %{rows: rows, columns: columns}), do:
    protocol
    |> Protocol.send_to_client({:row_description, columns, [:text]})
    |> send_rows(rows, columns, [:text])
    |> Protocol.send_to_client({:command_complete, "SELECT #{length(rows)}"})
  defp send_result(protocol, %{error: error}), do:
    Protocol.send_to_client(protocol, {:syntax_error, error})

  defp send_rows(protocol, rows, columns, formats), do:
    Enum.reduce(rows, protocol, &Protocol.send_to_client(&2, {:data_row, &1, column_types(columns), formats}))

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

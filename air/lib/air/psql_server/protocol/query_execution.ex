defmodule Air.PsqlServer.Protocol.QueryExecution do
  @moduledoc """
  Handles the querying part of PostgreSQL protocol.

  For more details see [here](https://www.postgresql.org/docs/9.6/static/protocol-flow.html#AEN112852) and
  [here](https://www.postgresql.org/docs/9.6/static/protocol-flow.html#PROTOCOL-FLOW-EXT-QUERY).
  """

  alias Air.PsqlServer.Protocol
  alias Air.PsqlServer.Protocol.Messages

  @behaviour Protocol


  # -------------------------------------------------------------------
  # Protocol callbacks
  # -------------------------------------------------------------------

  @impl Protocol
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
    |> Protocol.await_client_message()
  end
  def handle_client_message(protocol, :bind, bind_data) do
    prepared_statement = Map.fetch!(protocol.prepared_statements, bind_data.name)

    param_types =
      case prepared_statement.param_types do
        [_|_] -> prepared_statement.param_types
        [] -> prepared_statement.parsed_param_types
      end

    params = Messages.convert_params(bind_data.params, bind_data.format_codes, param_types)

    prepared_statement = %{prepared_statement | params: params, result_codes: bind_data.result_codes}

    protocol
    |> put_in([:portals, bind_data.portal], prepared_statement)
    |> Protocol.send_to_client(:bind_complete)
    |> Protocol.await_client_message()
  end
  def handle_client_message(protocol, :describe, describe_data) do
    describe_storage = describe_storage(describe_data.type)
    prepared_statement = protocol |> Map.fetch!(describe_storage) |> Map.fetch!(describe_data.name)

    %{protocol | describing_statement: {describe_data.type, describe_data.name}}
    |> Protocol.add_action({:describe_statement, prepared_statement.query, params_with_types(prepared_statement)})
    |> Protocol.next_state(:ready)
  end
  def handle_client_message(protocol, :execute, execute_data) do
    prepared_statement = Map.fetch!(protocol.portals, execute_data.portal)

    %{protocol | executing_portal: execute_data.portal}
    |> Protocol.add_action({:run_query, prepared_statement.query, params_with_types(prepared_statement),
      execute_data.max_rows})
    |> Protocol.next_state(:ready)
  end
  def handle_client_message(protocol, :sync, _), do:
    protocol
    |> Protocol.send_to_client(:ready_for_query)
    |> Protocol.await_client_message()
  def handle_client_message(protocol, :flush, _), do:
    Protocol.await_client_message(protocol, state: :ready)
  def handle_client_message(protocol, :close, close_data), do:
    protocol
    |> update_in([describe_storage(close_data.type)], &Map.delete(&1, close_data.name))
    |> Protocol.send_to_client(:close_complete)
    |> Protocol.await_client_message()

  @impl Protocol
  def handle_event(protocol, {:send_query_result, result}), do:
    protocol
    |> send_result(result)
    |> send_command_completion(result)
    |> send_ready_for_query(result)
    |> post_query_sync()
    |> Map.put(:executing_portal, nil)
    |> Protocol.await_client_message()
  def handle_event(protocol, {:describe_result, {:error, error}}), do:
    protocol
    |> Protocol.send_to_client({:syntax_error, error})
    |> Protocol.await_client_message()
  def handle_event(%{describing_statement: {describe_type, name}} = protocol, {:describe_result, description}) do
    describe_storage = describe_storage(describe_type)
    prepared_statement = protocol |> Map.fetch!(describe_storage) |> Map.fetch!(name)

    result_codes = prepared_statement.result_codes || [:text]
    protocol
    |> put_in([describe_storage, name, :parsed_param_types], Keyword.fetch!(description, :param_types))
    |> put_in([describe_storage, name, :columns], Keyword.fetch!(description, :columns))
    |> send_parameter_descriptions(prepared_statement, Keyword.fetch!(description, :param_types))
    |> send_row_description(Keyword.fetch!(description, :columns), result_codes)
    |> Protocol.await_client_message()
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp send_parameter_descriptions(protocol, %{params: nil}, param_types), do:
    # parameters are not bound -> send parameter descriptions
    Protocol.send_to_client(protocol, {:parameter_description, param_types})
  defp send_parameter_descriptions(protocol, _, _), do:
    # parameters are already bound -> client is not expecting parameter descriptions
    protocol

  defp send_result(protocol, {:error, :query_died}), do:
    Protocol.send_to_client(protocol, {:fatal_error, "Query unexpectedly quit."})
  defp send_result(protocol, {:error, :query_cancelled}), do:
    Protocol.send_to_client(protocol, :query_cancelled)
  defp send_result(protocol, {:error, error}), do:
    Protocol.send_to_client(protocol, {:syntax_error, error})
  defp send_result(%{executing_portal: nil} = protocol, result) do
    with {:ok, columns} <- Keyword.fetch(result, :columns),
         {:ok, rows} <- Keyword.fetch(result, :rows) do
      protocol
      |> send_row_description(columns, [:text])
      |> send_rows(rows, columns, [:text])
    else
      _ -> protocol
    end
  end
  defp send_result(protocol, result) do
    statement = Map.fetch!(protocol.portals, protocol.executing_portal)
    rows = Keyword.fetch!(result, :rows)
    send_rows(protocol, rows, statement.columns, statement.result_codes)
  end

  defp send_command_completion(protocol, {:error, _}), do:
    protocol
  defp send_command_completion(protocol, result), do:
    Protocol.send_to_client(protocol, {:command_complete, result_tag(result)})

  defp post_query_sync(%{executing_portal: nil} = protocol), do:
    protocol
  defp post_query_sync(protocol), do:
    Protocol.syncing(protocol)

  defp result_tag(result) do
    case Keyword.fetch(result, :rows) do
      {:ok, rows} ->
        "#{command_atom_to_string(Keyword.get(result, :command, :select))} #{length(rows)}"
      _ ->
        command_atom_to_string(Keyword.fetch!(result, :command))
    end
  end

  defp command_atom_to_string(tag_atom), do:
    tag_atom
    |> Atom.to_string()
    |> String.upcase()

  defp send_ready_for_query(protocol, {:error, _}), do:
    Protocol.send_to_client(protocol, :ready_for_query)
  defp send_ready_for_query(protocol, result) do
    if Keyword.get(result, :intermediate, false) do
      protocol
    else
      Protocol.send_to_client(protocol, :ready_for_query)
    end
  end

  defp send_row_description(protocol, [], _formats), do:
    Protocol.send_to_client(protocol, :no_data)
  defp send_row_description(protocol, columns, formats), do:
    Protocol.send_to_client(protocol, {:row_description, columns, formats})

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

  defp describe_storage(:statement), do: :prepared_statements
  defp describe_storage(:portal), do: :portals
end

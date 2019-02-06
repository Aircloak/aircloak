defmodule AirWeb.Socket.Cloak.MainChannel do
  @moduledoc """
  Main communication channel between a cloak and the air system.
  """
  use Phoenix.Channel, log_handle_in: false
  require Logger
  require Aircloak
  require Aircloak.DeployConfig

  alias Air.CentralClient.Socket

  @type views :: %{String.t() => String.t()}
  @type parameters :: nil | [map]
  @type described_columns :: [%{name: String.t(), type: String.t(), user_id: boolean}]

  @short_timeout :timer.seconds(20)

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Executes a query on the given cloak.

  The function returns when the cloak responds. If the timeout occurs, it is
  still possible that a cloak has received the request.
  """
  @spec run_query(pid, String.t(), String.t(), String.t(), String.t(), parameters, views) :: :ok | {:error, any}
  def run_query(channel_pid, query_id, analyst_id, statement, data_source_name, parameters, views) do
    payload = %{
      id: query_id,
      analyst_id: analyst_id,
      statement: statement,
      data_source: data_source_name,
      parameters: parameters,
      views: views
    }

    with {:ok, _} <- call(channel_pid, "run_query", encode(payload), @short_timeout), do: :ok
  end

  @doc """
  Asks the cloak to describe the query.

  Unlike `run_query/2`, this function is synchronous, meaning it waits for the
  cloak to respond, and returns the result obtained by the cloak.
  """
  @spec describe_query(pid, pos_integer, String.t(), String.t(), parameters, views) :: {:ok, map} | {:error, any}
  def describe_query(channel_pid, analyst_id, statement, data_source_name, parameters, views) do
    payload = %{
      analyst_id: analyst_id,
      statement: statement,
      data_source: data_source_name,
      parameters: parameters,
      views: views
    }

    call(channel_pid, "describe_query", encode(payload), @short_timeout)
  end

  @doc "Validates the view on the cloak."
  @spec validate_views(pid, String.t(), String.t(), views) :: map
  def validate_views(channel_pid, analyst_id, data_source_name, views) do
    payload = %{analyst_id: analyst_id, data_source: data_source_name, views: views}
    {:ok, results} = call(channel_pid, "validate_views", payload, @short_timeout)

    for result <- results, into: %{} do
      case result do
        %{name: name, valid: true, columns: columns} -> {name, {:ok, columns}}
        %{name: name, valid: false, field: field, error: error} -> {name, {:error, field, error}}
      end
    end
  end

  @doc "Stops a query on the given cloak."
  @spec stop_query(pid, String.t()) :: :ok | {:error, any}
  def stop_query(channel_pid, query_id) do
    with {:ok, _} <- call(channel_pid, "stop_query", query_id, @short_timeout), do: :ok
  catch
    :exit, _reason ->
      {:error, :disconnected}
  end

  @doc "Returns the list of queries running on this cloak."
  @spec running_queries(pid) :: {:ok, [String.t()]} | {:error, any}
  def running_queries(channel_pid), do: call(channel_pid, "running_queries", nil, :timer.minutes(4))

  @doc "Recreates the analyst table on cloaks."
  @spec create_or_update_analyst_table(pid, pos_integer, String.t(), String.t(), String.t(), parameters, views) ::
          {:ok, %{registration_info: String.t(), columns: described_columns}} | {:error, String.t()}
  def create_or_update_analyst_table(
        channel_pid,
        analyst_id,
        table_name,
        statement,
        data_source_name,
        parameters,
        views
      ) do
    payload = %{
      analyst_id: analyst_id,
      table_name: table_name,
      statement: statement,
      data_source: data_source_name,
      parameters: parameters,
      views: views
    }

    call(channel_pid, "create_or_update_analyst_table", payload, @short_timeout)
  end

  @doc "Asynchronously sends all known analyst tables to the cloak."
  @spec send_analyst_tables_to_cloak(pid) :: :ok
  def send_analyst_tables_to_cloak(channel_pid) do
    send(channel_pid, :send_analyst_tables)
    :ok
  end

  # -------------------------------------------------------------------
  # Phoenix.Channel callback functions
  # -------------------------------------------------------------------

  @impl Phoenix.Channel
  def join("main", cloak_info, socket) do
    with :ok <- validate_shared_secret(cloak_info[:secret_proof]) do
      Process.flag(:trap_exit, true)

      socket =
        socket
        |> assign(:pending_calls, %{})
        |> assign(:online_since, Timex.now())

      cloak = create_cloak(cloak_info, socket)

      cloak
      |> Air.Service.Cloak.register(cloak_info.data_sources)
      |> revalidate_views()

      Aircloak.in_env(test: :ok, else: send_analyst_tables_to_cloak(self()))

      {:ok, %{}, socket}
    else
      _ -> {:error, :cloak_secret_invalid}
    end
  end

  @impl Phoenix.Channel
  def terminate(_reason, socket) do
    cloak_id = socket.assigns.cloak_id
    Logger.info("cloak '#{cloak_id}' left air")
    {:ok, socket}
  end

  @impl Phoenix.Channel
  def handle_in("cloak_call", payload, socket),
    do:
      Aircloak.report_long({:handle_cloak_call, payload.event}, fn ->
        handle_cloak_call(payload.event, payload.payload, payload.request_id, socket)
      end)

  def handle_in(cloak_message_name, payload, socket),
    do:
      Aircloak.report_long({:handle_cloak_cast, cloak_message_name}, fn ->
        handle_cloak_message(cloak_message_name, payload, socket)
      end)

  @impl Phoenix.Channel
  def handle_info(message, socket),
    do: Aircloak.report_long({:handle_info, message}, fn -> handle_erlang_message(message, socket) end)

  # -------------------------------------------------------------------
  # Handling of messages from other processes
  # -------------------------------------------------------------------

  defp handle_erlang_message({{__MODULE__, :call}, timeout, from, event, payload}, socket) do
    request_id = make_ref() |> :erlang.term_to_binary() |> Base.encode64()
    push(socket, "air_call", %{request_id: request_id, event: event, payload: payload})
    timeout_ref = Process.send_after(self(), {:call_timeout, request_id}, timeout)

    {:noreply,
     assign(
       socket,
       :pending_calls,
       Map.put(socket.assigns.pending_calls, request_id, %{from: from, timeout_ref: timeout_ref})
     )}
  end

  defp handle_erlang_message({:call_timeout, request_id}, socket) do
    # We're just removing entries here without responding. It is the responsibility of the
    # client code to give up at some point.
    Logger.warn("#{request_id} sync call timeout on #{socket.assigns.cloak_id}")
    pending_calls = Map.delete(socket.assigns.pending_calls, request_id)
    {:noreply, assign(socket, :pending_calls, pending_calls)}
  end

  defp handle_erlang_message({:EXIT, _, :normal}, socket) do
    # probably the linked reporter terminated successfully
    {:noreply, socket}
  end

  defp handle_erlang_message(:send_analyst_tables, socket) do
    push(
      socket,
      "register_analyst_tables",
      %{registration_infos: Enum.map(Air.Service.AnalystTable.all(), & &1.result_info.registration_info)}
    )

    {:noreply, socket}
  end

  defp handle_erlang_message(message, socket) do
    cloak_id = socket.assigns.cloak_id
    Logger.info("unhandled info #{inspect(message)} from '#{cloak_id}'")
    {:noreply, socket}
  end

  # -------------------------------------------------------------------
  # Handling of Cloak messages
  # -------------------------------------------------------------------

  defp handle_cloak_message("memory_reading", reading, socket) do
    Air.Service.Cloak.Stats.record_memory(socket.assigns.cloak_id, Aircloak.atomize_keys(reading))
    {:noreply, socket}
  end

  defp handle_cloak_message("update_config", cloak_info, socket) do
    cloak_info
    |> create_cloak(socket)
    |> Air.Service.Cloak.update(cloak_info.data_sources)
    |> revalidate_views()

    {:noreply, socket}
  end

  defp handle_cloak_message("query_state", payload, socket) do
    Air.Service.Query.Lifecycle.state_changed(payload.query_id, payload.query_state)
    {:noreply, socket}
  end

  defp handle_cloak_message("cloak_response", payload, socket) do
    request_id = payload.request_id

    case Map.fetch(socket.assigns.pending_calls, request_id) do
      {:ok, request_data} ->
        Process.cancel_timer(request_data.timeout_ref)

        response =
          case payload.status do
            :ok -> {:ok, payload[:result]}
            :error -> {:error, payload[:result]}
            _other -> {:error, {:invalid_status, payload}}
          end

        respond_to_internal_request(request_data.from, response)

      :error ->
        Logger.warn("unknown sync call response: #{inspect(payload)}")
    end

    pending_calls = Map.delete(socket.assigns.pending_calls, request_id)
    {:noreply, assign(socket, :pending_calls, pending_calls)}
  end

  defp handle_cloak_message(event, _payload, socket) do
    cloak_id = socket.assigns.cloak_id
    Logger.warn("unknown event #{event} from '#{cloak_id}'")
    {:noreply, socket}
  end

  # -------------------------------------------------------------------
  # Handling cloak sync calls
  # -------------------------------------------------------------------

  defp handle_cloak_call("query_result", query_result, request_id, socket) do
    Logger.info("received result for query #{query_result.query_id}")
    respond_to_cloak(socket, request_id, :ok)
    Air.Service.Query.Lifecycle.result_arrived(query_result)
    Air.Service.DataSource.QueryScheduler.notify()
    {:noreply, socket}
  end

  defp handle_cloak_call(other, payload, request_id, socket) do
    Logger.warn("Received unknown cloak call #{inspect(other)} with payload #{inspect(payload, pretty: true)}")

    respond_to_cloak(socket, request_id, :ok)
    {:noreply, socket}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp validate_shared_secret(proof, shared_secret \\ Aircloak.DeployConfig.fetch!("site")["cloak_secret"])
  defp validate_shared_secret(_, nil), do: :ok
  defp validate_shared_secret(nil, _), do: :error
  defp validate_shared_secret(proof, shared_secret), do: Aircloak.SharedSecret.verify(proof, shared_secret)

  @spec respond_to_cloak(Socket.t(), request_id :: String.t(), :ok | :error, any) :: :ok
  defp respond_to_cloak(socket, request_id, status, result \\ nil) do
    push(socket, "air_response", %{
      request_id: request_id,
      status: status,
      result: result
    })
  end

  defp respond_to_internal_request({client_pid, mref}, response) do
    send(client_pid, {mref, response})
  end

  @spec call(pid | nil, String.t(), any, pos_integer) :: {:ok, any} | {:error, any}
  defp call(nil, _event, _payload, _timeout), do: {:error, :not_connected}

  defp call(pid, event, payload, timeout) do
    mref = Process.monitor(pid)
    send(pid, {{__MODULE__, :call}, timeout, {self(), mref}, event, payload})

    receive do
      {^mref, response} ->
        Process.demonitor(mref, [:flush])
        response

      {:DOWN, ^mref, _, _, reason} ->
        exit(reason)
    after
      timeout ->
        {:error, :timeout}
    end
  end

  defp create_cloak(cloak_info, socket),
    do: %{
      id: socket.assigns.cloak_id,
      name: socket.assigns.name,
      version: socket.assigns.version,
      online_since: socket.assigns.online_since,
      salt_hash: cloak_info.salt_hash
    }

  if Mix.env() == :test do
    # do nothing in tests, because it leads to unexpected messages in various tests where cloak is mocked
    defp revalidate_views(_data_sources), do: :ok
  else
    defp revalidate_views(data_sources), do: Enum.each(data_sources, &Air.Service.View.revalidate_all_views/1)
  end

  defp encode(query), do: update_in(query.parameters, &encode_parameters/1)

  defp encode_parameters(parameters) do
    parameters
    |> normalize_parameters()
    |> :erlang.term_to_binary()
    |> Base.encode16()
  end

  defp normalize_parameters(nil), do: nil
  defp normalize_parameters(params) when is_list(params), do: Enum.map(params, &normalize_parameter/1)

  defp normalize_parameter(param) do
    param = Aircloak.atomize_keys(param)
    with %{type: string} when is_binary(string) <- param, do: update_in(param.type, &String.to_existing_atom/1)
  end
end

defmodule Central.Service.Customer.AirMessage do
  @moduledoc "Decoding and handling of messages sent by air."
  require Logger
  alias Central.Service.Customer

  @type export :: %{
    id: integer,
    last_exported_id: integer,
    created_at: NaiveDateTime.t,
    air_name: String.t,
    customer_token: String.t,
    rpcs: [rpc]
  }

  @type rpc :: map

  @type import_error ::
    :invalid_format |
    :invalid_token |
    :already_imported |
    {:missing_previous_export, NaiveDateTime.t | nil}

  @type message_result :: any


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Handles an Air message"
  @spec handle(String.t, map, Customer.t, String.t) :: message_result
  def handle(message, payload, customer, air_name), do:
    do_handle(message, payload, customer, air_name)

  @doc "Decodes an Air export."
  @spec decode_exported_data(binary) :: {:ok, export} | {:error, :invalid_format}
  def decode_exported_data(air_export) do
    try do
      %{id: id, payload: payload, created_at: created_at} = :erlang.binary_to_term(air_export)

      %{
        "last_exported_id" => last_exported_id,
        "air_name" => air_name,
        "customer_token" => customer_token,
        "rpcs" => rpcs,
      } =
        payload
        |> :zlib.gunzip()
        |> Poison.decode!()

      {:ok, %{
        id: id,
        last_exported_id: last_exported_id,
        created_at: created_at,
        air_name: air_name,
        customer_token: customer_token,
        rpcs: rpcs,
      }}
    catch type, error ->
      Logger.error([
        "Error decoding Air data: #{inspect(type)}:#{inspect(error)}\n",
        Exception.format_stacktrace(System.stacktrace())
      ])
      {:error, :invalid_format}
    end
  end

  @doc "Validates an Air export."
  @spec validate_export(Customer.t, export) :: :ok | {:error, import_error}
  def validate_export(customer, export) do
    cond do
      Customer.imported?(customer, export.id) -> {:error, :already_imported}
      export.last_exported_id != nil && not Customer.imported?(customer, export.last_exported_id) ->
        last_export_time = case Customer.most_recent_export(customer) do
          nil -> nil
          export -> export.created_at
        end
        {:error, {:missing_previous_export, last_export_time}}
      true -> :ok
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp do_handle("query_execution", payload, customer, _air_name) do
    Logger.info("Received query execution update with payload: #{inspect payload}")
    params = %{
      metrics: payload["metrics"],
      features: payload["features"],
      aux: payload["aux"],
    }
    Customer.record_query(customer, params)
  end
  defp do_handle("cloak_online", cloak_info, customer, air_name) do
    Central.Service.Customer.update_cloak(customer, air_name,
      Map.fetch!(cloak_info, "name"),
      status: :online, data_source_names: Map.get(cloak_info, "data_source_names", []),
        version: Map.get(cloak_info, "version", "Unknown")
    )
    :ok
  end
  defp do_handle("cloak_offline", cloak_info, customer, air_name) do
    Central.Service.Customer.update_cloak(customer, air_name,
      Map.fetch!(cloak_info, "name"), status: :offline)
    :ok
  end
  defp do_handle("usage_info", uptime_info, customer, air_name) do
    Central.Service.Customer.store_uptime_info(
      customer,
      air_name,
      NaiveDateTime.from_iso8601!(Map.fetch!(uptime_info, "air_utc_time")),
      Map.delete(uptime_info, "air_utc_time")
    )
    :ok
  end
  defp do_handle(other, data, _customer, _air_name) do
    Logger.warn("unknown call `#{other}` (#{inspect(data)})")
    # Responding with ok, because the client can't fix this issue by retrying
    :ok
  end
end

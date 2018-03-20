defmodule Central.Service.Customer.AirMessage do
  @moduledoc "Decoding and handling of messages sent by air."
  require Logger
  alias Central.Service.Customer

  @type export :: %{
    id: integer,
    last_exported_id: integer,
    created_at: NaiveDateTime.t,
    air_name: String.t,
    air_version: String.t,
    customer_token: String.t,
    rpcs: [rpc]
  }

  @type rpc :: map

  @type import_error ::
    :invalid_format |
    :invalid_token |
    :already_imported |
    {:missing_previous_export, NaiveDateTime.t | nil}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Decodes an Air export."
  @spec decode_exported_data(binary) :: {:ok, export} | {:error, :invalid_format}
  def decode_exported_data(air_export) do
    %{id: id, payload: payload, created_at: created_at} = :erlang.binary_to_term(air_export)

    decoded = %{
      "last_exported_id" => last_exported_id,
      "air_name" => air_name,
      "license" => license,
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
      air_version: Map.get(decoded, "air_version", "Unknown"),
      license: license,
      rpcs: rpcs,
    }}
  catch type, error ->
    Logger.error([
      "Error decoding Air data: #{inspect(type)}:#{inspect(error)}\n",
      Exception.format_stacktrace(System.stacktrace())
    ])
    {:error, :invalid_format}
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
end

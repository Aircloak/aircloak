defmodule Central.Service.ElasticSearch do
  @moduledoc "Service module for interacting with elasticsearch"

  require Logger


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Records the execution of a query in elastic search.
  This makes a host of parameters available for analytics.
  Everything from the number of anonymized users have been
  queried for billing purposes, to the usage of different
  query features
  """
  @spec record_query(Customer.t, Map.t) :: :ok | :error
  def record_query(customer, params), do:
    record(:customer, :query,
      update_in(params, [:aux], &Map.put(&1 || %{}, :customer, %{id: customer.id, name: customer.name})))

  @doc "Records air presence in elastic search."
  @spec record_air_presence(Customer.t, String.t, :online | :offline) :: :ok | :error
  def record_air_presence(customer, air_name, status), do:
    record(:customer, :air, %{
      name: air_name,
      status: status,
      customer: %{id: customer.id, name: customer.name}
    })

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  if Mix.env == :test do
    defp record(_index, _type, _data), do: :ok
  else
    defp record(index, type, data) do
      elastic_endpoint = Central.site_setting("elastic_search_endpoint")
      url = "#{elastic_endpoint}/#{index}/#{type}"
      data = Map.put(data, :timestamp, Timex.format!(Timex.now(), "{ISO:Extended}"))
      case HTTPoison.post(url, Poison.encode!(data)) do
        {:ok, _} -> :ok
        other ->
          Logger.error("Got unexpected response from ElasticSearch: #{inspect other}")
          :error
      end
    end
  end
end

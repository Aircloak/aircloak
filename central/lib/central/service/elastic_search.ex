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
  def record_query(customer, params) do
    {:ok, timestamp} = Timex.format(Timex.now(), "{ISO:Extended}")
    aux = Map.get(params, :aux, %{})
      |> Map.put(:customer, %{id: customer.id, name: customer.name})
    params = params
      |> Map.put(:aux, aux)
      |> Map.put(:timestamp, timestamp)
    record(:customer, :query, params)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp record(index, type, data) do
    elastic_endpoint = Central.site_setting("elastic_search_endpoint")
    url = "#{elastic_endpoint}/#{index}/#{type}"
    case HTTPoison.post(url, Poison.encode!(data)) do
      {:ok, _} -> :ok
      other ->
        Logger.error("Got unexpected response from ElasticSearch: #{inspect other}")
        :error
    end
  end
end

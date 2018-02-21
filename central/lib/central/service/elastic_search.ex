defmodule Central.Service.ElasticSearch do
  @moduledoc "Service module for interacting with elasticsearch"

  require Logger
  alias Central.Schemas.Customer


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
    data = update_in(params, [:aux], &Map.put(&1 || %{}, :customer, %{id: customer.id, name: customer.name}))
    record(:customer, :query, data, parse_time(params[:aux]["finished_at"] || ""))
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp parse_time(string) do
    case NaiveDateTime.from_iso8601(string) do
      {:error, _} -> Timex.now()
      {:ok, result} -> result
    end
  end

  defp record(index, type, data, timestamp) do
    if Application.get_env(:central, :simulate_elastic?, false) do
      :ok
    else
      elastic_endpoint = Central.site_setting("elastic_search_endpoint")
      url = "#{elastic_endpoint}/#{index}/#{type}"
      data = Map.put(data, :timestamp, Timex.format!(timestamp, "{ISO:Extended:Z}"))
      case HTTPoison.post(url, Poison.encode!(data)) do
        {:ok, _} -> :ok
        other ->
          Logger.error("Got unexpected response from ElasticSearch: #{inspect other}")
          :error
      end
    end
  end
end

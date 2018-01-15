defmodule Central.Service.ElasticSearch do
  @moduledoc "Service module for interacting with elasticsearch"

  require Logger
  alias Central.Repo
  alias Central.Schemas.{Air, Customer}


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

  @doc "Records air presence in elastic search."
  @spec record_air_presence(Air.t) :: :ok | :error
  def record_air_presence(air) do
    air = Repo.preload(air, [:customer, :cloaks])

    record(:statuses, :air, %{
      name: air.name,
      status: air.status,
      online_cloaks: air.cloaks |> Enum.filter(&(&1.status == :online)) |> Enum.count(),
      customer: %{id: air.customer.id, name: air.customer.name}
    })

    Enum.each(air.cloaks, &record(:statuses, :cloak, %{
      name: &1.name,
      status: &1.status,
      data_source_names: &1.data_source_names,
      air_name: air.name,
      customer: %{id: air.customer.id, name: air.customer.name}
    }))
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

  defp record(index, type, data, timestamp \\ Timex.now()) do
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

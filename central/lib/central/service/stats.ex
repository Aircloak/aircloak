defmodule Central.Service.Stats do
  @moduledoc "Service module for working with stats"

  require Logger
  require Aircloak.DeployConfig

  import Ecto.Query, only: [from: 2]

  alias Central.Repo
  alias Central.Schemas.Customer

  #-----------------------------------------------------------------------------------------------------------
  # API functions
  #-----------------------------------------------------------------------------------------------------------

  @doc "Returns a list of customer, along with basic stats about their queries"
  @spec basic_stats() :: [Map.t]
  def basic_stats() do
    query = from customer in Customer,
      inner_join: query in assoc(customer, :queries),
      group_by: customer.id,
      select: %{
        customer_id: customer.id,
        customer_name: customer.name,
        query_count: count(query.id),
        users_count: sum(fragment("cast(?::json->>'users_count' as integer)", query.metrics))
      }
    Repo.all(query)
  end

  @doc "Returns a selection of stats for a given customer"
  @spec for_customer(Customer.t) :: Map.t
  def for_customer(customer) do
    query = from customer in Customer,
      inner_join: query in assoc(customer, :queries),
      where: customer.id == ^customer.id,
      select: %{
        query_count: count(query.id),
        users_count: sum(fragment("cast(?::json->>'users_count' as integer)", query.metrics)),
        average_row_count: avg(fragment("cast(?::json->>'row_count' as integer)",
          query.metrics)),
        average_execution_time: avg(fragment("cast(?::json->>'execution_time' as integer)",
          query.metrics)),
        max_execution_time: max(fragment("cast(?::json->>'execution_time' as integer)", query.metrics)),
      }
    Repo.one(query)
  end
end

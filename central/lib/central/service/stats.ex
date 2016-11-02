defmodule Central.Service.Stats do
  @moduledoc "Service module for working statts"

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
end

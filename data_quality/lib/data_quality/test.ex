defmodule DataQuality.Test do
  @moduledoc """
  Runs test queries against Aircloak and the un-anonymized query results,
  and compares the output quality.
  """
  alias DataQuality.Test.{Processing, Query, Persist, Present}

  @dimensions [
    {:dimension, "distribution"},
    {:dimension, "number"},
    {:bucket, 5},
    {:bucket, 10},
    {:bucket, 20},
    {:bucket, 50},
    {:bucket, 100},
    {:bucket, 500}
  ]
  @counts [
    {:count, "count(*)"},
    {:count, "count(distinct uid)"}
    # count(distinct column) is not supported by no-uid design yet so not yet included
  ]
  @mins [:min]
  @maxs [:max]
  @sums [:sum]
  # Note AVG doesn't yet work for no-uid, so is not included.

  @type aggregate_class :: String.t()
  @type distribution_name :: String.t()
  @type dimension :: {:dimension, String.t()} | {:bucket, integer}
  @type aggregate :: {:count, String.t()} | :min | :max | :sum
  @type data_source :: String.t()
  @type query_result :: %{data_source => number}
  @type test :: %{name: aggregate_class, aggregates: [aggregate]}

  @type results :: %{
          aggregate_class => %{
            distribution_name => %{
              dimension => %{
                aggregate => %{
                  raw_data: [query_result],
                  processed_data: %{mse: float | nil}
                }
              }
            }
          }
        }

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @spec run(Map.t()) :: :ok
  @doc "Performs data quality test"
  def run(config) do
    # Start dependencies so we can make HTTP requests
    Application.ensure_all_started(:inets)
    Application.ensure_all_started(:ssl)

    [
      %{
        name: "COUNT",
        aggregates: @counts
      },
      %{
        name: "MIN",
        aggregates: @mins
      },
      %{
        name: "MAX",
        aggregates: @maxs
      },
      %{
        name: "SUM",
        aggregates: @sums
      }
    ]
    |> Query.measure(config, @dimensions)
    |> Processing.calculate_mse()
    |> Present.mse(config)
    |> Persist.to_disk()

    :ok
  end
end

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

  @type category :: String.t()
  @type source :: String.t()
  @type global_results :: %{
          sources: [String.t()],
          mse_by_category: %{category => %{(dimension | aggregate) => [float]}},
          mse_by_source: %{source => float}
        }

  @type data_source_spec :: %{
          api_token: String.t(),
          data_source_name: String.t(),
          host: String.t(),
          name: String.t()
        }
  @type config :: %{
          database: %{
            database: String.t(),
            host: String.t(),
            user: String.t(),
            port: integer
          },
          anonymized: [data_source_spec],
          raw: data_source_spec
        }

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @spec run(config) :: :ok
  @doc "Performs data quality test"
  def run(config) do
    # Note AVG doesn't yet work for no-uid, so is not included.
    per_query_results =
      [
        %{
          name: "COUNT",
          aggregates: [
            {:count, "count(*)"},
            {:count, "count(distinct uid)"}
            # count(distinct column) is not supported by no-uid design yet so not yet included
          ]
        },
        %{
          name: "MIN",
          aggregates: [:min]
        },
        %{
          name: "MAX",
          aggregates: [:max]
        },
        %{
          name: "SUM",
          aggregates: [:sum]
        }
      ]
      |> Query.measure(config, @dimensions)
      |> Processing.calculate_mse()

    global_results = Processing.calculate_global_mse(per_query_results)
    Present.mse(per_query_results, global_results, config)

    Persist.to_disk(per_query_results)

    :ok
  end
end

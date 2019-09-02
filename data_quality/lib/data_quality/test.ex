defmodule DataQuality.Test do
  @moduledoc """
  Runs test queries against Aircloak and the un-anonymized query results,
  and compares the output quality.
  """
  alias DataQuality.Test.{Query, Persist, Present}

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

  @type dimension :: {:dimension, String.t()} | {:bucket, integer}
  @type aggregate :: {:count, String.t()} | :min | :max | :sum

  @type result :: %{
          aggregate: aggregate,
          class: String.t(),
          distribution: String.t(),
          dimension: dimension,
          source: String.t(),
          dimension_value: any,
          real_value: number,
          anonymized_value: number,
          relative_error: float,
          error: number
        }

  @type source :: String.t()

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

    [
      %{
        name: "COUNT",
        aggregates: [
          {:count, "count(*)"},
          {:count, "count(distinct uid)"},
          {:count, "count(distinct number)"}
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
    |> Present.mse()
    |> Persist.to_disk()

    :ok
  end
end

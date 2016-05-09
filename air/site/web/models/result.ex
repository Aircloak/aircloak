defmodule Air.Result do
  @moduledoc "The task result model."

  use Air.Web, :model

  alias Air.Task

  @type t :: %__MODULE__{}

  schema "results" do
    field :buckets, :string
    field :exceptions, :string
    field :post_processed, :string
    belongs_to :task, Task, type: Ecto.UUID

    timestamps
  end

  @required_fields ~w(buckets exceptions post_processed)
  @optional_fields ~w()

  @doc """
  Creates a changeset based on the `model` and `params`.

  If no params are provided, an invalid changeset is returned
  with no validation performed.
  """
  @spec changeset(t, %{String.t => term} | %{atom => term} | :empty) :: Ecto.Changeset.t
  def changeset(model, params \\ :empty) do
    model
    |> cast(params, @required_fields, @optional_fields)
  end

  @doc "Utility method to unpack the result's fields into a map for easier rendering in the UI."
  @spec unpack(t, pos_integer) :: %{}
  def unpack(result, bucket_count_limit) do
    limit_exceeded_bucket = %{
      "label" => "Notice",
      "value" => "Limit exceeded",
      "count" => "result is too large to be displayed"
    }
    # avoid processing buckets exceeding the size limit
    buckets = case String.length(result.buckets) > bucket_count_limit * 1024 do
      true -> [limit_exceeded_bucket]
      false -> Poison.decode!(result.buckets)
    end
    # avoid processing buckets exceeding the count limit
    buckets = case length(buckets) > bucket_count_limit do
      true -> [limit_exceeded_bucket]
      false -> buckets
    end
    # convert result buckets from maps to tuples
    buckets = for bucket <- buckets do
      {bucket["label"], bucket["value"], bucket["count"]}
    end

    exceptions = Poison.decode!(result.exceptions)
    errors = for exception <- exceptions do
          "#{exception["error"]} (#{exception["count"]} times)"
        end
        |> Enum.join("\n")

    %{
      id: result.id,
      timestamp: result.inserted_at,
      errors: errors,
      buckets: buckets
    }
  end
end

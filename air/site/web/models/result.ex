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
end

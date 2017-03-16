defmodule Central.Schemas.AirRPC do
  @moduledoc "Schema for Air RPCs. Allows us to ensure that perform an RPC at most once."

  use Central.Web, :model

  alias Ecto.Changeset

  @type t :: %__MODULE__{}

  @primary_key {:id, :string, []}
  @derive {Phoenix.Param, key: :id}
  schema "air_rpcs" do
    timestamps()
  end

  @required_fields ~w(id)a
  @optional_fields ~w()a


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Creates a changeset based on the `model` and `params`.

  If no params are provided, an invalid changeset is returned
  with no validation performed.
  """
  @spec changeset(t | Changeset.t, Map.t) :: Changeset.t
  def changeset(model, params \\ %{}) do
    model
    |> cast(params, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
  end
end


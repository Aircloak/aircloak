defmodule Air.Schemas.CentralCall do
  @moduledoc """
  This model holds RPC calls from the air to the central that failed
  due to the server not being connected, or other temporary glitches.
  """
  use Air.Schemas.Base

  alias Ecto.Changeset

  @type t :: %__MODULE__{}

  schema "central_calls" do
    field :event, :string
    field :payload, :map

    timestamps()
  end

  @required_fields ~w(event payload)a
  @optional_fields ~w()a


  # -------------------------------------------------------------------
  # API
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

  @doc """
  Creates a new instance of the `CentralCall` struct with the given event and payload.

  The new instance will have a weak unique id (unique only in the scope of this BEAM instance).
  """
  @spec new(String.t, map) :: t
  def new(event, payload) do
    now = NaiveDateTime.utc_now()
    %__MODULE__{id: :erlang.unique_integer, event: event, payload: payload, inserted_at: now, updated_at: now}
  end

  @doc "Exports a model instance into a JSON encodable map."
  @spec export(t) :: map
  def export(model), do:
    %{id: rpc_id(model), event: model.event, payload: model.payload}


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp rpc_id(model), do:
    # Generation of an almost unique id passed to the central. We can't use database ids, since in auto export
    # mode we're not storing RPCs to database. So instead, we're computing sha256 of the model. Since the
    # model consists of timestamps, payload content, and (at least weak) unique id (see new/2), it's very
    # unlikely that two different RPCs will have the same generated id. Even if that happens, the damage is
    # not big, since it will lead to one message not being imported into central - a property which already
    # exists, since we're not persisting pending RPCs in auto export mode.
    Base.encode64(:crypto.hash(:sha256, :erlang.term_to_binary(model)))
end

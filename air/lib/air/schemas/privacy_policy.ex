defmodule Air.Schemas.PrivacyPolicy do
  @moduledoc """
  The privacy policy model schema.
  Each privacy policy version gets a new database record.
  """

  use Ecto.Schema
  import Ecto.Changeset
  alias Air.Schemas.PrivacyPolicy

  @type t :: %__MODULE__{}

  schema "privacy_policies" do
    field(:content, :string)
    field(:changes, :string)

    timestamps()
  end

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc false
  def changeset(%PrivacyPolicy{} = privacy_policy, attrs) do
    privacy_policy
    |> cast(attrs, [:content, :changes])
    |> validate_required([:content])
  end
end

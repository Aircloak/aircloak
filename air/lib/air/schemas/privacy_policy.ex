defmodule Air.Schemas.PrivacyPolicy do
  use Ecto.Schema
  import Ecto.Changeset
  alias Air.Schemas.PrivacyPolicy

  @type t :: %__MODULE__{}

  schema "privacy_policies" do
    field(:content, :string)

    timestamps()
  end

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc false
  def changeset(%PrivacyPolicy{} = privacy_policy, attrs) do
    privacy_policy
    |> cast(attrs, [:content])
    |> validate_required([:content])
  end
end

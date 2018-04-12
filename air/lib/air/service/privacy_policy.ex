defmodule Air.Service.PrivacyPolicy do
  @moduledoc "Service for managing the privacy policy"

  alias Air.{Repo, Schemas.PrivacyPolicy}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns true if a privacy policy has been set"
  @spec exists?() :: boolean
  def exists?(), do: Repo.aggregate(PrivacyPolicy, :count, :id) > 0

  @doc "Records the rivacy policy. Should be used to create as well as edit the privacy policy as each edit consists of creating a new version"
  @spec set(String.t()) :: :ok
  def set(content) do
    %Air.Schemas.PrivacyPolicy{}
    |> Air.Schemas.PrivacyPolicy.changeset(%{content: content})
    |> Repo.insert!()

    :ok
  end
end

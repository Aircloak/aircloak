defmodule Air.Service.PrivacyPolicy do
  @moduledoc "Service for managing the privacy policy"

  alias Air.{Repo, Schemas.PrivacyPolicy}
  import Ecto.Query, only: [from: 2]

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

  @doc "Returns the current privacy policy"
  @spec get() :: {:ok, PrivacyPolicy.t()} | {:error, :no_privacy_policy_created}
  def get() do
    case Repo.one(from(pp in PrivacyPolicy, order_by: [desc: pp.inserted_at], limit: 1)) do
      nil ->
        {:error, :no_privacy_policy_created}

      privacy_policy ->
        {:ok, privacy_policy}
    end
  end
end

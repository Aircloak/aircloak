defmodule Air.Service.PrivacyPolicy do
  @moduledoc "Service for managing the privacy policy"

  alias Air.Repo
  alias Air.Schemas.PrivacyPolicy
  import Ecto.Query, only: [from: 2]

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns true if a privacy policy has been set"
  @spec exists?() :: boolean
  def exists?() do
    case get() do
      {:ok, _} -> true
      _ -> false
    end
  end

  @doc """
  Records the privacy policy.
  Should be used to create as well as edit the privacy policy as each edit consists of creating a new version
  """
  @spec set(String.t(), String.t() | nil) :: :ok
  def set(content, changes \\ nil) do
    %Air.Schemas.PrivacyPolicy{}
    |> Air.Schemas.PrivacyPolicy.changeset(%{content: content, changes: changes})
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

  @doc "Returns a privacy policy by it's revision number"
  @spec get_by_revision(pos_integer) :: {:ok, PrivacyPolicy.t()} | {:error, :not_found}
  def get_by_revision(revision) do
    case Repo.get(PrivacyPolicy, revision) do
      nil -> {:error, :not_found}
      privacy_policy -> {:ok, privacy_policy}
    end
  end

  @doc "Returns all privacy policies recorded in the system"
  @spec all() :: [PrivacyPolicy.t()]
  def all(), do: Repo.all(PrivacyPolicy)

  @doc "Returns the default privacy policy placeholder content"
  @spec default_content() :: String.t()
  def default_content(), do: "Placeholder content for a later default privacy policy"
end

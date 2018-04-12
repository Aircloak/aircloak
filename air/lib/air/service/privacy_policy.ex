defmodule Air.Service.PrivacyPolicy do
  @moduledoc "Service for managing the privacy policy"

  alias Air.{Repo, Schemas.PrivacyPolicy}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns true if a privacy policy has been set"
  @spec exists?() :: boolean
  def exists?(), do: Repo.aggregate(PrivacyPolicy, :count, :id) > 0
end

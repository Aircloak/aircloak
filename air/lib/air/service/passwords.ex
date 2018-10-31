defmodule Air.Service.Password do
  @moduledoc "Service module for working with passwords"

  alias Comeonin.Pbkdf2, as: Hash
  alias Air.Service.Password.Internal

  @type credentials :: %{login: String.t(), hash: String.t()}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Produces a password hash that can be stored into the database for later validation."
  @spec hash(String.t()) :: String.t()
  def hash(password), do: Hash.hashpwsalt(password)

  @doc "Validates whether a password"
  @spec validate(String.t(), nil | String.t()) :: boolean
  def validate(_password, nil), do: Hash.dummy_checkpw()
  def validate(password, hash), do: Hash.checkpw(password, hash)
end

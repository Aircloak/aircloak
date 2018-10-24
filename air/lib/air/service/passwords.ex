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

  @doc "Processes a passwords accounts file into a list of user and password maps"
  @spec process_credentials(String.t()) :: [credentials()]
  def process_credentials(content) do
    content
    |> Internal.parse_credentials()
    |> Enum.map(&hash_credential_password/1)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp hash_credential_password(credential),
    do:
      credential
      |> Map.put(:hash, hash(credential.password))
      |> Map.delete(:password)
end

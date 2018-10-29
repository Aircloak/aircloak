defmodule Aircloak.SharedSecret do
  @moduledoc "A module for proving ownership of a shared secret over a potentially unsecure channel."

  alias Comeonin.Pbkdf2, as: Hash

  @doc "Generates a proof of possession of the shared secret."
  @spec proof(String.t()) :: String.t()
  def proof(secret), do: Hash.hashpwsalt(secret)

  @doc "Verifies that the provided proof indeed proves possesion of the shared secret."
  @spec verify(String.t(), String.t()) :: :ok | :error
  def verify(proof, secret), do: if(Hash.checkpw(secret, proof), do: :ok, else: :error)
end

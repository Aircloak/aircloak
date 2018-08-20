defmodule Air.Service.License.FSM do
  @moduledoc """
  Represents the license state of the system. It starts out with no license (system setup). Later license texts can be
  submitted to update that to a valid license.
  """

  @type t :: :no_license | Aircloak.License.t()

  @doc "Returns the initial state representing no license."
  @spec initial() :: t
  def initial(), do: :no_license

  @doc """
  Tries to load the given license text using the given public key for decryption. Returns `{:ok, new_license}` if
  loading succeeded or `{:error, old_state}` if it failed.
  """
  @spec load(t, ExPublicKey.RSAPublicKey.t(), String.t()) :: {:ok, t} | {:error, t}
  def load(state, public_key, encrypted_license) do
    case Aircloak.License.decrypt(public_key, encrypted_license) do
      {:ok, license} -> {:ok, Map.put(license, :text, encrypted_license)}
      :error -> {:error, state}
    end
  end

  @doc "Returns true if the given state represents a loaded license (either valid or expired), false otherwise."
  @spec present?(t) :: boolean
  def present?(:no_license), do: false
  def present?(_), do: true

  @doc "Returns true if the given state represents a valid license, false otherwise."
  @spec valid?(t) :: boolean
  def valid?(state), do: Timex.diff(expiry(state), Timex.now()) > 0

  @doc "Returns true if the given states represents a renewing license, false otherwise."
  @spec auto_renew?(t) :: boolean
  def auto_renew?(:no_license), do: false
  def auto_renew?(license), do: license.auto_renew

  @doc "Returns the expiry time for the given state."
  @spec expiry(t) :: DateTime.t()
  def expiry(:no_license), do: Timex.now() |> Timex.shift(years: -1)
  def expiry(state), do: state.expires_at

  @doc "Returns the list of features for the given state."
  @spec features(t) :: [atom]
  def features(:no_license), do: []
  def features(state), do: state.features

  @doc "Returns the original text of the license before decryption."
  @spec text(t) :: String.t()
  def text(:no_license), do: ""
  def text(state), do: state.text
end

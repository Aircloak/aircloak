defmodule Air.Service.License.FSM do
  def initial(), do: :no_license

  def load(state, public_key, encrypted_license) do
    case Aircloak.License.decrypt(public_key, encrypted_license) do
      {:ok, license} -> {:ok, Map.put(license, :text, encrypted_license)}
      :error -> {:error, state}
    end
  end

  def present?(:no_license), do: false
  def present?(_), do: true

  def valid?(state), do: Timex.diff(expiry(state), Timex.now()) > 0

  def expiry(:no_license), do: Timex.now() |> Timex.shift(years: -1)
  def expiry(state), do: state.expires_at

  def customer_id(:no_license), do: nil
  def customer_id(state), do: state.customer_id

  def license_id(:no_license), do: nil
  def license_id(state), do: state.license_id

  def text(:no_license), do: ""
  def text(state), do: state.text
end

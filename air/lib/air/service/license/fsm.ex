defmodule Air.Service.License.FSM do
  def initial(), do: :no_license

  def load(state, public_key, encrypted_license) do
    encrypted_license
    |> String.split("\n")
    |> Enum.map(fn(line) ->
      with {:ok, plain_text} <- ExPublicKey.decrypt_public(line, public_key),
           {:ok, map} <- Poison.decode(plain_text),
           {:ok, license} <- unpack(map)
      do
        {:ok, license}
      else
        _ -> nil
      end
    end)
    |> Enum.find({:error, state}, & &1)
  end

  def license_present?(:no_license), do: false
  def license_present?(_), do: true

  def expired?(state), do: Timex.diff(expiry(state), Timex.now()) < 0

  def expiry(:no_license), do: Timex.now() |> Timex.shift(years: -1)
  def expiry(state), do: state.expires_at

  def customer_id(:no_license), do: nil
  def customer_id(state), do: state.customer_id

  def license_id(:no_license), do: nil
  def license_id(state), do: state.license_id

  defp unpack(%{"customer_id" => customer_id, "id" => license_id, "expires_at" => expires_at}) do
    case Timex.parse(expires_at, "{ISO:Basic}") do
      {:ok, expires_at} -> {:ok, %{customer_id: customer_id, license_id: license_id, expires_at: expires_at}}
      _ -> :error
    end
  end
  defp unpack(_), do: :error
end

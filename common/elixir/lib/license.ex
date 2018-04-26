defmodule Aircloak.License do
  @moduledoc "Allows for decrypting an aircloak license file."

  @type t :: %{
          customer_id: any,
          license_id: any,
          expires_at: DateTime.t()
        }

  @doc "Decrypts the given aircloak license text with the given public key."
  @spec decrypt(ExPublicKey.RSAPublicKey.t(), String.t()) :: {:ok, t} | :error
  def decrypt(public_key, encrypted_license) do
    encrypted_license
    |> String.split("\n")
    |> Enum.map(fn line ->
      with {:ok, plain_text} <- line |> String.trim() |> ExPublicKey.decrypt_public(public_key),
           {:ok, map} <- Poison.decode(plain_text),
           {:ok, license} <- unpack(map) do
        {:ok, license}
      else
        _ -> nil
      end
    end)
    |> Enum.find(:error, & &1)
  end

  defp unpack(%{"customer_id" => customer_id, "id" => license_id, "expires_at" => expires_at}) do
    case Timex.parse(expires_at, "{ISO:Basic}") do
      {:ok, expires_at} ->
        {:ok, %{customer_id: customer_id, license_id: license_id, expires_at: expires_at}}

      _ ->
        :error
    end
  end

  defp unpack(_), do: :error
end

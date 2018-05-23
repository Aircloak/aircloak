defmodule Central.Guardian do
  @moduledoc "Implementation module for Guardian."
  use Guardian, otp_app: :central

  import Ecto.Query, only: [from: 2]
  alias Central.{Repo, Schemas.User}

  @doc false
  def subject_for_token(user = %User{}, _claims), do: {:ok, "User:#{user.id}"}
  def subject_for_token(_, _), do: {:error, "Unknown resource type"}

  @doc false
  def resource_from_claims(%{"sub" => "User:" <> id}) do
    case Repo.one(from(user in User, where: user.id == ^id)) do
      nil -> {:error, "User doesn't exist"}
      user -> {:ok, user}
    end
  end

  def resource_from_claims(_), do: {:error, "Unknown resource type"}
end

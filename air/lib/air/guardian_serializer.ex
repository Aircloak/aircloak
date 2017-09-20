defmodule Air.GuardianSerializer do
  @moduledoc false
  @behaviour Guardian.Serializer

  import Ecto.Query, only: [from: 2]
  alias Air.{Repo, Schemas.User}

  # Temporarily suppress dialyzer due to typespec error in Guardian
  @dialyzer {:nowarn_function, for_token: 1}

  def for_token(user = %User{}), do: {:ok, "User:#{user.id}"}
  def for_token(_), do: {:error, "Unknown resource type"}

  def from_token("User:" <> id) do
    case Repo.one(from user in User, where: user.id == ^id, preload: [:groups]) do
      nil -> {:error, "User doesn't exist"}
      user -> {:ok, user}
    end
  end
  def from_token(_), do: {:error, "Unknown resource type"}
end

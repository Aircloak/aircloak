defmodule Central.GuardianSerializer do
  @moduledoc false
  @behaviour Guardian.Serializer

  import Ecto.Query, only: [from: 2]
  alias Central.Repo
  alias Central.Schemas.User

  # Temporarily suppress dialyzer due to typespec error in Guardian
  @dialyzer {:nowarn_function, for_token: 1}

  def for_token(user = %User{}), do: {:ok, "User:#{user.id}"}
  def for_token(_), do: {:error, "Unknown resource type"}

  def from_token("User:" <> id) do
    user = Repo.one!(from(user in User, where: user.id == ^id))
    {:ok, user}
  end

  def from_token(_), do: {:error, "Unknown resource type"}
end

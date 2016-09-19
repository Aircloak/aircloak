defmodule Air.GuardianSerializer do
  @moduledoc false
  @behaviour Guardian.Serializer

  import Ecto.Query, only: [from: 2]
  alias Air.{Repo, User}

  # Temporarily suppress dialyzer due to typespec error in Guardian
  @dialyzer {:nowarn_function, for_token: 1}

  def for_token(user = %User{}), do: {:ok, "User:#{user.id}"}
  def for_token(_), do: {:error, "Unknown resource type"}

  def from_token("User:" <> id) do
    user = Repo.one!(
      from user in User,
      join: organisation in assoc(user, :organisation),
      where: user.id == ^id,
      preload: [organisation: organisation]
    )
    |> Repo.preload([:groups])
    {:ok, user}
  end
  def from_token(_), do: {:error, "Unknown resource type"}
end

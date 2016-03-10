defmodule Air.GuardianSerializer do
  @moduledoc false
  @behaviour Guardian.Serializer

  alias Air.Repo
  alias Air.User

  # Temporarily suppress dialyzer due to typespec error in Guardian
  @dialyzer {:nowarn_function, for_token: 1}

  def for_token(user = %User{}), do: { :ok, "User:#{user.id}" }
  def for_token(_), do: { :error, "Unknown resource type" }

  def from_token("User:" <> id), do: { :ok, Repo.get(User, id) }
  def from_token(_), do: { :error, "Unknown resource type" }
end

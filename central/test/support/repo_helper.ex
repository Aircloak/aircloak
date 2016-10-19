defmodule Central.TestRepoHelper do
  @moduledoc "Helpers for working with the repository."

  alias Central.Repo

  @doc "Inserts the new user with default parameters into the database."
  @spec create_user!(%{}) :: Central.User.t
  def create_user!(additional_changes \\ %{}) do
    Central.User.changeset(%Central.User{}, %{
      email: "#{random_string()}@aircloak.com",
      password: "1234",
      password_confirmation: "1234",
      name: random_string()
    })
    |> Central.User.changeset(additional_changes)
    |> Repo.insert!()
  end

  defp random_string,
    do: Base.encode16(:crypto.strong_rand_bytes(10))
end

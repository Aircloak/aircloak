defmodule Air.TestRepoHelper do
  @moduledoc "Helpers for working with the repository."

  @doc "Inserts the new organisation into the database."
  @spec create_organisation! :: Air.Organisation.t
  def create_organisation! do
    %Air.Organisation{}
    |> Air.Organisation.changeset(%{name: random_string()})
    |> Air.Repo.insert!()
  end

  @doc "Inserts the new user into the database."
  @spec create_user!(Air.Organisation.t, Air.User.role_key) :: Air.User.t
  def create_user!(organisation, role_key) do
    organisation
    |> Ecto.build_assoc(:users)
    |> Air.User.changeset(%{
          email: "#{random_string()}@aircloak.com",
          password: "1234",
          password_confirmation: "1234",
          name: random_string(),
          role_id: Air.User.role_id(role_key)
        })
    |> Air.Repo.insert!()
  end

  defp random_string,
    do: Base.encode16(:crypto.strong_rand_bytes(10))
end

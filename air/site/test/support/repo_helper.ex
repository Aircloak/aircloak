defmodule Air.TestRepoHelper do
  @moduledoc "Helpers for working with the repository."

  @doc "Inserts the new organisation into the database."
  @spec create_organisation!(String.t) :: Air.Organisation.t
  def create_organisation!(name \\ random_string()) do
    %Air.Organisation{}
    |> Air.Organisation.changeset(%{name: name})
    |> Air.Repo.insert!()
  end

  @doc "Retrieves the administrators organisation."
  @spec admin_organisation :: Air.Organisation.t
  def admin_organisation do
    Air.Repo.get_by!(Air.Organisation, name: Air.Organisation.admin_group_name())
  end

  @doc "Inserts the new user into the database."
  @spec create_user!(Air.Organisation.t, Air.User.role_key) :: Air.User.t
  def create_user!(organisation, role_key \\ :user) do
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

  @doc "Inserts a new user with default parameters into the database. See create_user!/2 for details"
  @spec create_user!() :: Air.User.t
  def create_user!() do
    org = create_organisation!()
    create_user!(org, :user)
  end

  @doc "Inserts a test task into the database"
  @spec create_task!(Air.User.t, %{}) :: Air.Task.t
  def create_task!(user, params \\ %{name: "name", query: "query content", permanent: true}) do
    user
    |> Ecto.build_assoc(:tasks)
    |> Air.Task.changeset(params)
    |> Air.Repo.insert!()
  end

  defp random_string,
    do: Base.encode16(:crypto.strong_rand_bytes(10))
end

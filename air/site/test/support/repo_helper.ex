defmodule Air.TestRepoHelper do
  @moduledoc "Helpers for working with the repository."

  alias Air.{ApiToken, Repo}

  @doc "Inserts the new organisation into the database."
  @spec create_organisation!(String.t) :: Air.Organisation.t
  def create_organisation!(name \\ random_string()) do
    %Air.Organisation{}
    |> Air.Organisation.changeset(%{name: name})
    |> Repo.insert!()
  end

  @doc "Retrieves the administrators organisation."
  @spec admin_organisation :: Air.Organisation.t
  def admin_organisation do
    Repo.get_by!(Air.Organisation, name: Air.Organisation.admin_group_name())
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
    |> Repo.insert!()
  end

  @doc "Inserts a new user with default parameters into the database. See create_user!/2 for details"
  @spec create_user!() :: Air.User.t
  def create_user!() do
    org = create_organisation!()
    create_user!(org, :user)
  end

  @doc "Creates a group with default parameters with a random group name to avoid clashes"
  @spec create_group!() :: Air.Group.t
  def create_group!() do
    %Air.Group{}
    |> Air.Group.changeset(%{name: "group-#{random_string()}"})
    |> Repo.insert!()
  end

  @doc "Creates a data source with default parameters with a random global id"
  @spec create_data_source!() :: Air.DataSource.t
  def create_data_source!() do
    params = %{
      global_id: "global_id-#{random_string()}",
      name: "name-#{random_string()}",
      tables: "[]"
    }
    %Air.DataSource{}
    |> Air.DataSource.changeset(params)
    |> Repo.insert!()
  end

  @doc "Inserts a new token with default parameters into the database."
  @spec create_token!() :: Air.ApiToken.t
  @spec create_token!(Air.User.t) :: Air.ApiToken.t
  def create_token!(user \\ create_user!()) do
    ApiToken.changeset(%ApiToken{}, %{user_id: user.id, description: "some description"})
    |> Repo.insert!
  end

  @doc "Inserts a test query into the database"
  @spec create_query!(Air.User.t, %{}) :: Air.Query.t
  def create_query!(user, params \\ %{query: "query content"}) do
    user
    |> Ecto.build_assoc(:queries)
    |> Air.Query.changeset(params)
    |> Repo.insert!()
  end

  defp random_string,
    do: Base.encode16(:crypto.strong_rand_bytes(10))
end

defmodule Air.TestRepoHelper do
  @moduledoc "Helpers for working with the repository."

  alias Air.{ApiToken, Repo}

  @doc "Inserts the new user with default parameters into the database."
  @spec create_user!() :: Air.User.t
  def create_user!() do
    Air.User.changeset(%Air.User{}, %{
      email: "#{random_string()}@aircloak.com",
      password: "1234",
      password_confirmation: "1234",
      name: random_string()
    })
    |> Repo.insert!()
    |> Repo.preload([:groups])
  end

  @doc "Creates a user that is an admin. See create_user!/0 and make_admin!/1"
  @spec create_admin_user!() :: Air.User.t
  def create_admin_user!() do
    create_user!() |> make_admin!()
  end

  @doc "Creates a group with default parameters with a random group name to avoid clashes"
  @spec create_group!() :: Air.Group.t
  def create_group!() do
    %Air.Group{}
    |> Air.Group.changeset(%{name: "group-#{random_string()}", admin: false})
    |> Repo.insert!()
  end

  @doc "Adds a group with admin rights to the user"
  @spec make_admin!(Air.User.t) :: Air.User.t
  def make_admin!(user) do
    admin_group = %Air.Group{}
    |> Air.Group.changeset(%{name: "admin-group-#{random_string()}", admin: true})
    |> Repo.insert!()
    user
    |> Air.User.changeset(%{groups: [admin_group.id]})
    |> Repo.update!()
    |> Repo.preload([:groups])
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
    |> Repo.insert!()
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

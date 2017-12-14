defmodule Air.TestRepoHelper do
  @moduledoc "Helpers for working with the repository."

  alias Air.{Service.User, Service.Query, Schemas.ApiToken, Schemas.Group, Repo}

  @doc "Inserts the new user with default parameters into the database."
  @spec create_user!(%{}) :: Air.Schemas.User.t
  def create_user!(additional_changes \\ %{}), do:
    %{
      email: "#{random_string()}@aircloak.com",
      password: "1234",
      password_confirmation: "1234",
      name: random_string()
    }
    |> Map.merge(additional_changes)
    |> User.create!()
    |> Repo.preload([:groups])

  @doc "Creates a user that is an admin. See create_user!/0 and make_admin!/1"
  @spec create_admin_user!() :: Air.Schemas.User.t
  def create_admin_user!() do
    create_user!() |> make_admin!()
  end

  @doc "Creates an admin user, and deletes all other users."
  @spec create_only_user_as_admin!() :: Air.Schemas.User.t
  def create_only_user_as_admin!() do
    previous_users = User.all()
    admin = create_admin_user!()
    Enum.each(previous_users, &User.delete!/1)
    admin
  end

  @doc "Creates a group with default parameters with a random group name to avoid clashes"
  @spec create_group!(map()) :: Group.t
  def create_group!(additional_changes \\ %{}), do:
    User.create_group!(Map.merge(%{name: "group-#{random_string()}", admin: false}, additional_changes))

  @doc "Adds a group with admin rights to the user"
  @spec make_admin!(Air.Schemas.User.t) :: Air.Schemas.User.t
  def make_admin!(user) do
    admin_group = create_group!(%{admin: true})

    user
    |> User.update!(%{groups: [admin_group.id]})
    |> Repo.preload([:groups])
  end

  @doc "Creates a data source with default parameters with a random name"
  @spec create_data_source!(%{}) :: Air.Schemas.DataSource.t
  def create_data_source!(additional_changes \\ %{}) do
    params = %{
      name: "name_#{random_string()}",
      tables: "[]"
    }
    Air.Service.DataSource.create!(Map.merge(params, additional_changes))
    |> Repo.preload([:groups])
  end

  @doc "Inserts a new token with default parameters into the database."
  @spec create_token!() :: Air.Schemas.ApiToken.t
  @spec create_token!(Air.Schemas.User.t) :: Air.Schemas.ApiToken.t
  def create_token!(user \\ create_user!()) do
    ApiToken.changeset(%ApiToken{}, %{user_id: user.id, access: :api, description: "some description"})
    |> Repo.insert!()
  end

  @doc "Inserts a new monitoring token the database."
  @spec create_monitoring_token!() :: String.t
  def create_monitoring_token!() do
    user = create_admin_user!()
    token = %ApiToken{}
    |> ApiToken.changeset(%{user_id: user.id, access: :monitoring, description: "some description"})
    |> Repo.insert!()

    salt = Application.get_env(:air, AirWeb.Endpoint) |> Keyword.fetch!(:api_token_salt)
    Phoenix.Token.sign(AirWeb.Endpoint, salt, token.id)
  end

  @doc "Inserts a test query into the database"
  @spec create_query!(Air.Schemas.User.t, %{}) :: Air.Schemas.Query.t
  def create_query!(user, params \\ %{statement: "query content", session_id: Ecto.UUID.generate()}), do:
    user
    |> Ecto.build_assoc(:queries)
    |> Air.Schemas.Query.changeset(Map.merge(%{context: :http}, params))
    |> Repo.insert!()

  @doc "Registers a cloak a serving a data source, returning the data source id"
  @spec create_and_register_data_source() :: String.t
  def create_and_register_data_source() do
    data_source_name = "data_source_id_#{:erlang.unique_integer()}"
    do_register_data_source!(data_source_name)
    data_source_name
  end

  @doc "Registers a cloak serving the given data source name."
  @spec register_data_source!(DataSource.t) :: :ok
  def register_data_source!(data_source), do:
    do_register_data_source!(data_source.name)

  @doc "Retrieves a query from the database by id."
  @spec get_query(String.t) :: {:ok, Air.Schemas.Query.t} | {:error, :not_found}
  def get_query(id) do
    case Air.Repo.get(Air.Schemas.Query, id) do
      nil -> {:error, :not_found}
      query -> {:ok, query}
    end
  end

  @doc "Returns parameters expected when registering a cloak in the air"
  @spec cloak_info() :: Map.t
  def cloak_info(name \\ "cloak_name") do
    %{
      id: "cloak_id_#{:erlang.unique_integer()}",
      name: name,
      online_since: Timex.now(),
      version: "17.1.0",
    }
  end

  @doc "Creates and returns a view, for a given data source"
  @spec create_view!(User.t, DataSource.t, String.t) :: View.t
  def create_view!(user, data_source, view_name \\ random_string()) do
    %Air.Schemas.View{}
    |> Ecto.Changeset.cast(
      %{
        user_id: user.id,
        data_source_id: data_source.id,
        name: view_name,
        sql: "sql for #{view_name}",
        result_info: %{"columns" => ["foo", "bar"]},
      },
      ~w(name sql user_id data_source_id result_info)a
    )
    |> Repo.insert!()
  end

  @doc "Encodes a list of rows into the cloak format."
  @spec encode_rows(nil | [map]) :: nil | binary
  def encode_rows(nil), do:
    nil
  def encode_rows(result) do
    case result[:rows] do
      nil -> nil
      rows ->
        rows
        |> Poison.encode_to_iodata!()
        |> :zlib.gzip()
    end
  end

  @doc "Sends the query result to the Query service."
  @spec send_query_result(String.t, map, [map]) :: :ok
  def send_query_result(query_id, result, rows \\ nil), do:
    result
    |> Map.put(:query_id, query_id)
    |> Map.put(:chunks, encode_chunks(rows))
    |> Map.put(:row_count, row_count(rows))
    |> Query.process_result()


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp random_string,
    do: Base.encode16(:crypto.strong_rand_bytes(10))

  defp do_register_data_source!(name) do
    data_sources = [%{name: name, tables: []}]
    Air.Service.Cloak.register(cloak_info(), data_sources)
    :ok
  end

  defp encode_chunks(nil), do:
    []
  defp encode_chunks(rows), do:
    rows
    |> Stream.chunk(1000, 1000, [])
    |> Stream.with_index()
    |> Enum.map(&encode_chunk/1)

  defp encode_chunk({rows, index}), do:
    %{
      index: index,
      encoded_data: rows |> :jiffy.encode([:use_nil]) |> :zlib.gzip()
    }

  defp row_count(nil), do:
    nil
  defp row_count(rows), do:
    rows |> Stream.map(&(&1.occurrences)) |> Enum.sum()
end

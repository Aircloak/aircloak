defmodule Air.API.DataSourceController.Test do
  # Despite using Ecto 2.0 with it's transactional DB sandbox model,
  # we have to run these tests sequentially.
  # The problem causing the sequential execution is that the database pool
  # is used from a process distinct from the test one:
  #
  # Ecto provides two options:
  #
  # - explicitly allowing a third process to access a sandbox pool
  # - sharing the test pool with the world
  #
  # Explicitly allowing the DataSourceManager access doesn't work as
  # we would have to concurrently give it access to multiple test pools,
  # which then in turns means it wouldn't know which to check out a connection from.
  #
  # Using distinct servers per test could work, but would require polluting the
  # GenServer with test specific code.
  #
  # The sharing option is the one we are using, but since any process can access
  # the pool, we cannot run tests concurrently.
  use Air.ConnCase, async: false

  import Air.{TestConnHelper, TestRepoHelper}
  alias Air.{TestSocketHelper, DataSource, Repo}
  alias Poison, as: JSON

  setup do
    Ecto.Adapters.SQL.Sandbox.mode(Repo, {:shared, self()})
    :ok
  end

  test "getting all sources" do
    organisation = admin_organisation()
    user = create_user!(organisation)
    api_token = create_token!(user)
    cloak_name = "cloak_name"

    [api_cloak] = TestSocketHelper.with_cloak(cloak_name, organisation.name, "data_source_name", fn ->
      api_conn(api_token)
      |> get("/api/data_sources")
      |> response(200)
      |> JSON.decode!()
    end)

    [data_source] = Repo.all(DataSource)
    assert api_cloak["name"] == "data_source_name"
    assert api_cloak["tables"] == []
    assert api_cloak["id"] == data_source.id
  end
end

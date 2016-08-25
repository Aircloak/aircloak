defmodule Air.API.DataSourceController.Test do
  use Air.ConnCase

  import Air.{TestConnHelper, TestRepoHelper}
  alias Air.{TestSocketHelper, DataSource, Repo}
  alias Poison, as: JSON

  setup do
    Repo.delete_all(DataSource)
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

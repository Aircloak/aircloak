defmodule Air.API.DataSourceController.Test do
  use Air.ConnCase

  import Air.{TestConnHelper, TestRepoHelper}
  alias Air.TestSocketHelper
  alias Poison, as: JSON

  test "getting all sources" do
    organisation = admin_organisation()
    user = create_user!(organisation)
    api_token = create_token!(user)
    cloak_name = "cloak_name"

    result = TestSocketHelper.with_cloak(cloak_name, organisation.name, "data_source_name", fn ->
      api_conn(api_token)
      |> get("/api/data_sources")
      |> response(200)
      |> JSON.decode!()
    end)

    assert [%{"cloak_name" => "cloak_name", "id" => "data_source_name@cloak_name"}] = result
  end
end

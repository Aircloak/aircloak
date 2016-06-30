defmodule Air.API.DataSourceController.Test do
  use Air.ConnCase

  import Air.{TestConnHelper, TestRepoHelper}
  alias Air.{Token, TestSocketHelper}
  alias Poison, as: JSON

  test "getting all sources" do
    organisation = admin_organisation()
    user = create_user!(organisation)
    api_token = create_token!(user)

    result = TestSocketHelper.with_cloak("cloak_name", organisation.name, "data_source_name", fn ->
      api_conn(api_token)
      |> get("/api/data_sources")
      |> response(200)
      |> JSON.decode!()
    end)

    assert [%{"display" => "data_source_name (cloak_name)", "token" => token}] = result
    assert {"unknown_org/cloak_name", "data_source_name"} = Token.decode_data_source_token(token)
  end
end

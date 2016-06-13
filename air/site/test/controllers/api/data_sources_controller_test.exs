defmodule Air.API.DataSourcesController.Test do
  use Air.ConnCase

  import Air.{TestConnHelper, TestRepoHelper}
  alias Air.{Token, TestSocketHelper}
  alias Phoenix.Channels.GenSocketClient.TestSocket
  alias Poison, as: JSON

  test "getting all sources" do
    user = create_user!(admin_organisation())
    api_token = create_token!(user)

    result = with_cloak(:cloak_name, :data_source_name, fn ->
      api_conn(api_token)
      |> get("/api/data_sources")
      |> response(200)
      |> JSON.decode!()
    end)

    assert [%{"name" => "data_source_name cloak_name", "token" => token}] = result
    assert {"unknown_org/cloak_name", "data_source_name"} = Token.decode_data_source_token(token)
  end

  defp with_cloak(cloak_name, data_source_name, action) do
    socket = TestSocketHelper.connect!(%{cloak_name: cloak_name})

    try do
      data_source = %{id: data_source_name, tables: []}
      TestSocketHelper.join!(socket, "main", %{name: cloak_name, data_sources: [data_source]})
      action.()
    after
      TestSocket.leave(socket, "main")
    end
  end
end

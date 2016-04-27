defmodule Air.API.TaskControllerTest do
  use Air.ConnCase
  import Air.{TestAuthHelper, TestRepoHelper}

  test "authenticates properly", %{conn: conn} do
    conn = add_auth_to_conn(conn)
    token = create_token(conn, create_user!())

    %{"success" => success} = conn
        |> put_req_header("auth-token", token)
        |> post("/api/task")
        |> response(200)
        |> Poison.decode!

    assert success
  end
end

defmodule Air.API.AuthTest do
  use Air.ConnCase

  import Air.{TestRepoHelper, TestAuthHelper}

  test "should be notified about auth-token requirement", %{conn: conn} do
    %{"success" => success, "description" => description} = conn
        |> post("/api/task") |> response(401) |> Poison.decode!
    refute success
    assert description =~ "authenticated with auth-token"
  end

  test "should be notified about incorrect auth-tokens", %{conn: conn} do
    %{"success" => success, "description" => description} = conn
        |> put_req_header("auth-token", "FOOBAR")
        |> post("/api/task") |> response(401) |> Poison.decode!
    refute success
    assert description =~ "Invalid auth-token"
  end

  test "valid authentication should assign user to conn", %{conn: conn} do
    user = create_user!()
    conn = add_auth_to_conn(conn)
    token = create_token(conn, user)

    updated_conn = conn
        |> put_req_header("auth-token", token)
        |> post("/api/task")
    assert updated_conn.assigns.current_user.id == user.id
  end
end

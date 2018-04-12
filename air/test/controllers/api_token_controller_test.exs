defmodule AirWeb.ApiTokenControllerTest do
  use AirWeb.ConnCase, async: true

  import Air.{TestConnHelper, TestRepoHelper, TestAuthHelper}
  alias Air.Schemas.ApiToken

  setup do
    create_privacy_policy!()
    :ok
  end

  test "api token pages require an authenticated user", %{conn: conn} do
    conn = add_auth_to_conn(conn)
    user = create_user!()
    token = create_token(user)

    assert "/auth" == build_conn() |> get(api_token_path(conn, :index)) |> redirected_to()
    assert "/auth" == delete(conn, api_token_path(conn, :delete, token)) |> redirected_to()
  end

  test "index only shows tokens owned by the user", %{conn: conn} do
    conn = add_auth_to_conn(conn)
    user = create_user!()
    user_token = create_token_entity!(user)
    other_user = create_user!()
    other_user_token = create_token_entity!(other_user)

    index_html = login(user) |> get(api_token_path(conn, :index)) |> response(200)
    assert index_html =~ api_token_path(conn, :delete, user_token)
    refute index_html =~ api_token_path(conn, :delete, other_user_token)
  end

  test "deleting tokens of other users is forbidden", %{conn: conn} do
    conn = add_auth_to_conn(conn)
    user = create_user!()
    other_user = create_user!()
    other_user_token = create_token_entity!(other_user)

    not_found_html = login(user) |> delete(api_token_path(conn, :delete, other_user_token)) |> response(404)

    assert not_found_html =~ "Not found"
  end

  test "deletes chosen api token", %{conn: conn} do
    conn = add_auth_to_conn(conn)
    user = create_user!()
    token = create_token_entity!(user)

    assert "/api_tokens" == login(user) |> delete(api_token_path(conn, :delete, token)) |> redirected_to()

    refute Repo.get(ApiToken, token.id)
  end
end

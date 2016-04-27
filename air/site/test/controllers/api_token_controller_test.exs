defmodule Air.ApiTokenControllerTest do
  use Air.ConnCase

  import Air.TestConnHelper
  alias Air.{TestRepoHelper, TestSocketHelper, ApiToken}

  @invalid_attrs %{name: ""}
  @api_token_params %{api_token: %{description: "Token description"}}

  defp create_user do
    org = TestRepoHelper.create_organisation!()
    TestRepoHelper.create_user!(org, :user)
  end

  defp create_token(user) do
    changeset = ApiToken.changeset(%ApiToken{}, %{description: "Test description", user_id: user.id})
    {:ok, token} = Repo.insert(changeset)
    token
  end

  test "api token pages require an authenticated user", %{conn: conn} do
    user = create_user()
    token = create_token(user)

    assert "/auth" == conn() |> get(api_token_path(conn, :index)) |> redirected_to()
    assert "/auth" == delete(conn, api_token_path(conn, :delete, token)) |> redirected_to()
  end

  test "index only shows tokens owned by the user", %{conn: conn} do
    user = create_user()
    user_token = create_token(user)
    other_user = create_user()
    other_user_token = create_token(other_user)

    index_html = login(user) |> get(api_token_path(conn, :index)) |> response(200)
    assert index_html =~ api_token_path(conn, :delete, user_token)
    refute index_html =~ api_token_path(conn, :delete, other_user_token)
  end

  test "deleting tokens of other users is forbidden", %{conn: conn} do
    user = create_user()
    other_user = create_user()
    other_user_token = create_token(other_user)

    not_found_html = login(user) |> delete(api_token_path(conn, :delete, other_user_token)) |> response(404)
    assert not_found_html =~ "not found"
  end

  test "deletes chosen api token", %{conn: conn} do
    user = create_user()
    token = create_token(user)

    assert "/api_tokens" == login(user) |> delete(api_token_path(conn, :delete, token)) |> redirected_to()
    refute Repo.get(ApiToken, token.id)
  end
end

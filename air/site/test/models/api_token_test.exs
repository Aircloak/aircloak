defmodule Air.ApiTokenTest do
  use Air.ModelCase
  alias Air.{ApiToken, Repo}
  alias Plug.Conn
  import Air.{TestAuthHelper, TestRepoHelper}

  @valid_attrs %{description: "some content", user_id: 1}
  @invalid_attrs %{}

  test "changeset with valid attributes" do
    changeset = ApiToken.changeset(%ApiToken{}, @valid_attrs)
    assert changeset.valid?
  end

  test "changeset with invalid attributes" do
    changeset = ApiToken.changeset(%ApiToken{}, @invalid_attrs)
    refute changeset.valid?
  end

  test "should create unique tokens for users" do
    conn = add_auth_to_conn(%Conn{})
    user = create_user!()
    token1 = ApiToken.create_for(conn, user, "description")
    token2 = ApiToken.create_for(conn, user, "description")
    assert token1 != token2
  end

  test "should not validate invalid tokens" do
    assert :error = ApiToken.user_for_token(add_auth_to_conn(%Conn{}), "invalid token")
  end

  test "should be able to find user for a valid token" do
    conn = add_auth_to_conn(%Conn{})
    user = create_user!()
    token = ApiToken.create_for(conn, user, "description")
    returned_user = ApiToken.user_for_token(conn, token)
    assert user.id == returned_user.id
  end

  test "should return error for revoked tokens" do
    conn = add_auth_to_conn(%Conn{})
    user = create_user!()
    token = ApiToken.create_for(conn, user, "description")
    Repo.delete_all(ApiToken)
    assert :error == ApiToken.user_for_token(conn, token)
  end
end

defmodule Air.Token.Test do
  use Air.ModelCase
  alias Air.{ApiToken, Token, Repo}
  alias Plug.Conn
  import Air.{TestAuthHelper, TestRepoHelper}

  test "should create unique tokens for users" do
    conn = add_auth_to_conn(%Conn{})
    user = create_user!()
    token1 = Token.create_api_token(conn, user, "description")
    token2 = Token.create_api_token(conn, user, "description")
    assert token1 != token2
  end

  test "should not validate invalid tokens" do
    assert :error = Token.user_for_token(add_auth_to_conn(%Conn{}), "invalid token")
  end

  test "should be able to find user for a valid token" do
    conn = add_auth_to_conn(%Conn{})
    user = create_user!()
    token = Token.create_api_token(conn, user, "description")
    returned_user = Token.user_for_token(conn, token)
    assert user.id == returned_user.id
  end

  test "should return error for revoked tokens" do
    conn = add_auth_to_conn(%Conn{})
    user = create_user!()
    token = Token.create_api_token(conn, user, "description")
    Repo.delete_all(ApiToken)
    assert :error == Token.user_for_token(conn, token)
  end
end

defmodule Air.Token.Test do
  use Air.ModelCase, async: true
  alias Air.{Schemas.ApiToken, Token, Repo}
  import Air.{TestRepoHelper}

  test "should create unique tokens for users" do
    user = create_user!()
    token1 = Token.create_api_token(user, "description")
    token2 = Token.create_api_token(user, "description")
    assert token1 != token2
  end

  test "should not validate invalid tokens" do
    assert :error = Token.user_for_token("invalid token")
  end

  test "should be able to find user for a valid token" do
    user = create_user!()
    token = Token.create_api_token(user, "description")
    returned_user = Token.user_for_token(token)
    assert user.id == returned_user.id
  end

  test "should return error for revoked tokens" do
    user = create_user!()
    token = Token.create_api_token(user, "description")
    Repo.delete_all(ApiToken)
    assert :error == Token.user_for_token(token)
  end
end

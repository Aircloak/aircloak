defmodule Air.Token.Test do
  use Air.SchemaCase, async: false
  alias Air.{Schemas.ApiToken, Token, Repo}
  import Air.{TestRepoHelper}

  setup do
    Ecto.Adapters.SQL.Sandbox.mode(Repo, {:shared, self()})
    :ok
  end

  setup [:create_user]

  test "should create unique tokens for users", %{user: user} do
    token1 = Token.create_api_token(user, :api, "description")
    token2 = Token.create_api_token(user, :api, "description")
    assert token1 != token2
  end

  describe "user_for_token" do
    test "should not validate invalid tokens" do
      assert :error = Token.user_for_token("invalid token", :api)
    end

    test "should be able to find user for a valid token", %{user: user} do
      token = Token.create_api_token(user, :api, "description")
      returned_user = Token.user_for_token(token, :api)
      assert user.id == returned_user.id
    end

    test "should return error for revoked tokens", %{user: user} do
      token = Token.create_api_token(user, :api, "description")
      Repo.delete_all(ApiToken)
      assert :error == Token.user_for_token(token, :api)
    end

    test "should return error for disabled users", %{user: user} do
      token = Token.create_api_token(user, :api, "description")
      {:ok, _} = Air.Service.User.disable(user)
      assert :error = Token.user_for_token(token, :api)
    end

    test "trying to access a different access category", %{user: user} do
      token = Token.create_api_token(user, :api, "description")
      assert :error = Token.user_for_token(token, :monitoring)
    end
  end

  def create_user(context), do: {:ok, Map.put(context, :user, create_user!())}
end

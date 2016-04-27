defmodule Air.ApiTokenTest do
  use Air.ModelCase

  alias Air.{ApiToken, Repo, User}

  @valid_attrs %{description: "some content", user_id: 1}
  @invalid_attrs %{}

  test "changeset with valid attributes" do
    changeset = User.changeset(%User{}, @valid_attrs)
  end

  test "changeset with valid attributes" do
    changeset = ApiToken.changeset(%ApiToken{}, @valid_attrs)
    assert changeset.valid?
  end

  test "changeset with invalid attributes" do
    changeset = ApiToken.changeset(%ApiToken{}, @invalid_attrs)
    refute changeset.valid?
  end

  test "should create unique tokens for users" do
    user = create_user()
    token1 = ApiToken.create_for(conn(), user, "description")
    token2 = ApiToken.create_for(conn(), user, "description")
    assert token1 != token2
  end

  test "should not validate invalid tokens" do
    assert :error = ApiToken.user_for_token(conn(), "invalid token")
  end

  test "should be able to find user for a valid token" do
    user = create_user()
    token = ApiToken.create_for(conn(), user, "description")
    returned_user = ApiToken.user_for_token(conn(), token)
    assert user.id == returned_user.id
  end

  test "should return error for revoked tokens" do
    user = create_user()
    token = ApiToken.create_for(conn(), user, "description")
    Repo.delete_all(ApiToken)
    assert :error == ApiToken.user_for_token(conn(), token)
  end

  defp create_user do
    changeset = User.changeset(%User{}, %{
          email: "test@example.com",
          password: "1234",
          password_confirmation: "1234",
          name: "Test user",
          organisation_id: 1,
          role_id: 0
        })
    {:ok, user} = Repo.insert(changeset)
    user
  end


  # -------------------------------------------------------------------
  # Infrastructure mocking
  # -------------------------------------------------------------------

  defmodule TokenEndpoint do
    def config(:secret_key_base), do: "abc123"
  end

  defp conn() do
    %Plug.Conn{} |> Plug.Conn.put_private(:phoenix_endpoint, TokenEndpoint)
  end
end

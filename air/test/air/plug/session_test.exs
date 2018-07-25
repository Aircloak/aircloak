defmodule AirWeb.Plug.Session.Test do
  use AirWeb.ConnCase, async: false

  alias Air.{Token, TestRepoHelper}
  alias AirWeb.Plug.Session.ApiAuth

  describe "ApiAuth" do
    test "submitting an invalid token" do
      conn = build_conn() |> ApiAuth.call(%{})

      assert conn.halted
      assert conn.status == 401
    end

    test "submitting a token for an disabled user" do
      user = TestRepoHelper.create_user!()
      token = Token.create_api_token(user, :api, "test")
      {:ok, _} = Air.Service.User.disable!(user)

      conn =
        build_conn()
        |> Plug.Conn.put_req_header("auth-token", token)
        |> ApiAuth.call(access: :api)

      assert conn.halted
      assert conn.status == 401
    end

    test "submitting a token through a header" do
      user = TestRepoHelper.create_user!()
      token = Token.create_api_token(user, :api, "test")

      conn =
        build_conn()
        |> Plug.Conn.put_req_header("auth-token", token)
        |> ApiAuth.call(access: :api)

      refute conn.halted
      assert conn.assigns.current_user.id == user.id
    end

    test "submitting a token through a param" do
      user = TestRepoHelper.create_user!()
      token = Token.create_api_token(user, :api, "test")

      conn =
        build_conn()
        |> put_in([Access.key!(:query_string)], "auth_token=#{token}")
        |> ApiAuth.call(access: :api)

      refute conn.halted
      assert conn.assigns.current_user.id == user.id
    end
  end
end

defmodule Air.Plug.Session.Test do
  use Air.ConnCase, async: false

  alias Air.{Token, TestRepoHelper, Plug.Session.ApiAuth}

  describe "ApiAuth" do
    test "submitting an invalid token" do
      conn = build_conn() |> ApiAuth.call(%{})

      assert conn.halted
      assert conn.status == 401
    end

    test "submitting a token for a given user" do
      user = TestRepoHelper.create_user!()
      token = Token.create_api_token(user, "test")

      conn = build_conn() |> Plug.Conn.put_req_header("auth-token", token) |> ApiAuth.call(%{})

      refute conn.halted
      assert conn.assigns.current_user.id == user.id
    end
  end
end

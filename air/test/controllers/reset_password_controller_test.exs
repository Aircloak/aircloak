defmodule Air.ResetPasswordController.Test do
  use AirWeb.ConnCase, async: true

  alias Air.TestRepoHelper
  alias Air.Service.User

  setup do
    user = TestRepoHelper.create_user!()
    token = User.reset_password_token(user)
    {:ok, user: user, token: token}
  end

  describe "update" do
    test "successful reset signs in the user", %{user: user, token: token} do
      conn =
        build_conn()
        |> put("/reset_password", %{
          token: token,
          user: %{password: "psswrd12", password_confirmation: "psswrd12"}
        })
        |> recycle()
        |> get("/profile/edit")

      assert response(conn, 200) =~ user.name
    end

    test "resetting with invalid token does not sign in" do
      conn =
        build_conn()
        |> put("/reset_password", %{
          token: "whatever",
          user: %{password: "psswrd12", password_confirmation: "psswrd12"}
        })
        |> recycle()
        |> get("/profile/edit")

      assert redirected_to(conn) == "/auth"
    end

    test "resetting with invalid confirmation does not sign in", %{token: token} do
      conn =
        build_conn()
        |> put("/reset_password", %{
          token: token,
          user: %{password: "psswrd12", password_confirmation: "password12345"}
        })
        |> recycle()
        |> get("/profile/edit")

      assert redirected_to(conn) == "/auth"
    end
  end
end

defmodule Air.Service.Password.Internal.Test do
  use Air.SchemaCase, async: true

  alias Air.Service.Password

  describe ".parse_credentials" do
    test "ignores garbage" do
      assert [] == Password.Internal.parse_credentials("FOO\nBAR")
    end

    test "parses username and password" do
      assert [
               %{login: "login", password: "password"}
             ] = Password.Internal.parse_credentials("login:password")
    end

    test "parses multiple users" do
      assert [_, _] = Password.Internal.parse_credentials("\nl1:p1\nl2:p2\n\n")
    end

    test "allows for an additional admin paramter" do
      content = """
      login1:password1:admin
      login2:password2
      """

      assert [user1, user2] = Password.Internal.parse_credentials(content)

      assert user1.admin
      refute user2.admin
    end

    test "ignores third parameters that aren't admin" do
      content = "login1:password1:foobar"
      assert [user] = Password.Internal.parse_credentials(content)
      refute user.admin
    end
  end
end

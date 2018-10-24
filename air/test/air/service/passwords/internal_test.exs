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
             ] == Password.Internal.parse_credentials("login:password")
    end

    test "parses multiple users" do
      assert [_, _] = Password.Internal.parse_credentials("\nl1:p1\nl2:p2\n\n")
    end
  end
end

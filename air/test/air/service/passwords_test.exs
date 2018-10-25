defmodule Air.Service.Password.Test do
  use Air.SchemaCase, async: true

  alias Air.Service.Password

  @password "12345678"

  describe ".hash" do
    test "hashed password is different from plain text" do
      refute @password == Password.hash(@password)
    end

    test "hashed passwords differ" do
      refute Password.hash(@password <> "1") == Password.hash(@password <> "2")
    end
  end

  describe ".validate" do
    test "succesfully validates password against hash" do
      assert Password.validate(@password, Password.hash(@password))
    end

    test "fails on invalid hash" do
      refute Password.validate(@password, Password.hash(@password) <> "foo")
    end

    test "fails on missing hash" do
      refute Password.validate(@password, nil)
    end
  end

  describe ".process_credentials" do
    test "creates list of users with password validateable hashes from file content" do
      content = """
      USERS
      =====

      login1:password1
      login2:password2
      """

      assert [
               %{login: "login1", hash: hash1},
               %{login: "login2", hash: hash2}
             ] = Password.process_credentials(content)

      assert Password.validate("password1", hash1)
      assert Password.validate("password2", hash2)
    end

    test "captures whether a user is an admin" do
      content = "login1:password1:admin"
      assert [%{admin: true}] = Password.process_credentials(content)
    end
  end
end

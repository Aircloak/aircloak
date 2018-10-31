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

    test "passwords hashed multiple times differ" do
      refute Password.hash(@password) == Password.hash(Password.hash(@password))
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
end

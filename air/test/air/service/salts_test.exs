defmodule Air.Service.Salts.Test do
  use Air.SchemaCase

  alias Air.Service.Salts

  describe ".get" do
    test "raises for unknown name" do
      assert_raise RuntimeError, fn -> Salts.get(:unknown) end
    end

    test "generates the same salt for the same name" do
      salt = Salts.get(:api_token)
      assert Salts.get(:api_token) == salt
    end

    test "generates different salt for different names" do
      refute Salts.get(:api_token) == Salts.get(:password_reset)
    end
  end
end

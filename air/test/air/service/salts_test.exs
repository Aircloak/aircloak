defmodule Air.Service.Salts.Test do
  use AirWeb.ConnCase

  alias Air.Service.Salts

  describe ".get" do
    test "raises for unknown name" do
      assert_raise RuntimeError, fn -> Salts.get(:unknown) end
    end

    test "generates the same salt for the same name" do
      assert Salts.get(:api_token) == Salts.get(:api_token)
    end

    test "generates different salt for different names" do
      refute Salts.get(:api_token) == Salts.get(:password_reset)
    end

    test "the salts are pesistent" do
      {:ok, first_instance} = Agent.start_link(fn -> %{} end)
      {:ok, second_instance} = Agent.start_link(fn -> %{} end)

      assert Salts.get(first_instance, :api_token) == Salts.get(second_instance, :api_token)
    end
  end
end

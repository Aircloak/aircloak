defmodule Air.Service.Salts.Test do
  use Air.SchemaCase

  alias Air.Service.Salts

  describe ".get" do
    test "raises for unknown name" do
      server = start_server()
      assert_raise RuntimeError, fn -> Salts.get(server, :unknown) end
    end

    test "generates the same salt for the same name" do
      server = start_server()
      salt = Salts.get(server, :api_token)
      assert Salts.get(server, :api_token) == salt
    end

    test "generates different salt for different names" do
      server = start_server()
      refute Salts.get(server, :api_token) == Salts.get(server, :password_reset)
    end

    test "the salts are pesistent" do
      first_instance = start_server()
      second_instance = start_server()

      assert Salts.get(first_instance, :api_token) == Salts.get(second_instance, :api_token)
    end

    defp start_server() do
      {:ok, server} = Agent.start_link(fn -> %{} end)
      server
    end
  end
end

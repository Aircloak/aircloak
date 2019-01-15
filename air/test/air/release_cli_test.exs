defmodule Air.ReleaseCLI.Test do
  use Air.SchemaCase, async: false

  alias Air.ReleaseCLI
  import ExUnit.CaptureIO

  describe ".reset_password" do
    test "with invalid login" do
      assert capture_io(fn ->
               ReleaseCLI.reset_password("does not exist")
             end) =~ ~r/A user with that login does not exist/
    end

    test "with valid login" do
      user = Air.TestRepoHelper.create_user!()
      login = Air.Service.User.main_login(user)

      output = capture_io(fn -> ReleaseCLI.reset_password(login) end)
      [_, token] = Regex.run(~r/Use the following token.*:\s+(.*)\n/, output)

      assert {:ok, %{id: id}} =
               Air.Service.User.reset_password(token, %{
                 password: "password1234",
                 password_confirmation: "password1234"
               })

      assert id == user.id
    end
  end
end

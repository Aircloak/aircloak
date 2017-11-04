defmodule CentralWeb.Socket.Frontend.UserChannelTest do
  alias CentralWeb.Socket.Frontend.UserChannel

  import Central.TestRepoHelper

  use CentralWeb.ChannelCase, async: true

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Central.Repo)
    user = create_user!()
    {:ok, _, _} =
      socket("user", %{user: user})
      |> subscribe_and_join(UserChannel, "user:" <> to_string(user.id))

    {:ok, user: user}
  end
end

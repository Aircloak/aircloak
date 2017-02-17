defmodule Air.Socket.Frontend.UserChannelTest do
  alias Air.Socket.Frontend.UserChannel

  import Air.TestRepoHelper

  use Air.ChannelCase, async: true

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Air.Repo)
  end

  setup do
    user = create_user!()
    session = Ecto.UUID.generate()
    {:ok, _, _} =
      socket("user", %{user: user})
      |> subscribe_and_join(UserChannel, "session:" <> session)

    {:ok, socket: socket(), user: user, session: session}
  end

  test "allows joining session with any UUID", %{socket: socket} do
    assert {:ok, _, _} = subscribe_and_join(socket, UserChannel, "session:#{Ecto.UUID.generate()}")
  end

  test "does not allow joining sessions that are not valid UUIDs", %{socket: socket} do
    assert {:error, _} = subscribe_and_join(socket, UserChannel, "session:some_id")
  end

  test "results of queries are pushed to the given session", %{user: user, session: session_id} do
    query = create_query!(user, %{session_id: session_id})
    UserChannel.broadcast_result(query)

    expected = Air.Schemas.Query.for_display(query)

    assert_push("result", ^expected)
  end

  test "results of queries are not pushed to other sessions", %{user: user} do
    query = create_query!(user, %{session_id: Ecto.UUID.generate()})
    result = Air.Schemas.Query.for_display(query)

    UserChannel.broadcast_result(query)

    refute_push("result", ^result)
  end
end

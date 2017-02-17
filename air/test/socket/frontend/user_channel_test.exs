defmodule Air.Socket.Frontend.UserChannelTest do
  alias Air.Socket.Frontend.UserChannel

  import Air.TestRepoHelper

  use Air.ChannelCase, async: true

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Air.Repo)
  end

  describe "Joining session channel" do
    setup [:with_user, :with_session, :with_socket]

    test "allows joining session with any UUID", %{socket: socket} do
      assert {:ok, _, _} = subscribe_and_join(socket, UserChannel, "session:#{Ecto.UUID.generate()}")
    end

    test "does not allow joining sessions that are not valid UUIDs", %{socket: socket} do
      assert {:error, _} = subscribe_and_join(socket, UserChannel, "session:some_id")
    end
  end

  describe "Query results on session channel" do
    setup [:with_user, :with_session, :with_socket, :subscribed_to_sessions]

    test "receive results on session channel", %{user: user, session: session_id} do
      query = create_query!(user, %{session_id: session_id})
      UserChannel.broadcast_result(query)
      expected = Air.Schemas.Query.for_display(query)
      assert_push("result", ^expected)
    end

    test "no results for queries belonging to other sessions", %{user: user} do
      query = create_query!(user, %{session_id: Ecto.UUID.generate()})
      result = Air.Schemas.Query.for_display(query)
      UserChannel.broadcast_result(query)
      refute_push("result", ^result)
    end
  end

  defp with_user(_), do: {:ok, user: create_user!()}

  defp with_session(_), do: {:ok, session: Ecto.UUID.generate()}

  defp with_socket(context), do: {:ok, socket: socket("user", %{user: context[:user]})}

  defp subscribed_to_sessions(context) do
    {:ok, _, _} = subscribe_and_join(context[:socket], UserChannel, "session:" <> context[:session])
    :ok
  end
end

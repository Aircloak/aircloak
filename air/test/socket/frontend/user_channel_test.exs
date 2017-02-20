defmodule Air.Socket.Frontend.UserChannelTest do
  alias Air.Socket.Frontend.UserChannel

  import Air.TestRepoHelper

  use Air.ChannelCase, async: false

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Air.Repo)
    Ecto.Adapters.SQL.Sandbox.mode(Air.Repo, {:shared, self()})
    :ok
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

  describe "Subscribing to state changes channel" do
    test "can't join when not an admin" do
      user = create_user!()
      {:ok, [socket: socket]} = with_socket(%{user: user})
      assert {:error, _} = subscribe_and_join(socket, UserChannel, "state_changes:all")
    end

    test "can join as an admin" do
      user = create_user!()
      admin = make_admin!(user)
      {:ok, [socket: socket]} = with_socket(%{user: admin})
      assert {:ok, _, _} = subscribe_and_join(socket, UserChannel, "state_changes:all")
    end
  end

  describe "Receiving query states" do
    setup [:with_user, :made_admin, :with_socket, :subscribed_to_all_state_changes]

    test "receive query data for new query" do
      data_source = create_data_source!()
      some_user = create_user!()
      query = create_query!(some_user, %{completed: true, data_source_id: data_source.id})
      Air.QueryEvents.StateChanges.trigger_event(query.id, :started)
      assert_push("state_change", message)
      assert message[:query_id] == query.id
      assert message[:event] == :started
      assert message[:query].id == query.id
    end

    test "receive event when query completes", context do
      query = create_query!(context[:user])
      Air.QueryEvents.StateChanges.trigger_event(query.id, :completed)
      assert_push("state_change", message)
      assert message[:query_id] == query.id
      assert message[:event] == :completed
    end
  end

  defp with_user(_), do: {:ok, user: create_user!()}

  defp made_admin(context), do: {:ok, user: make_admin!(context[:user])}

  defp with_session(_), do: {:ok, session: Ecto.UUID.generate()}

  defp with_socket(context), do: {:ok, socket: socket("user", %{user: context[:user]})}

  defp subscribed_to_sessions(context) do
    {:ok, _, _} = subscribe_and_join(context[:socket], UserChannel, "session:" <> context[:session])
    :ok
  end

  defp subscribed_to_all_state_changes(context) do
    {:ok, _, _} = subscribe_and_join(context[:socket], UserChannel, "state_changes:all")
    :ok
  end
end

defmodule AirWeb.Socket.Frontend.UserChannelTest do
  alias AirWeb.Socket.Frontend.UserChannel

  import Air.TestRepoHelper

  use AirWeb.ChannelCase, async: false

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Air.Repo)
    Ecto.Adapters.SQL.Sandbox.mode(Air.Repo, {:shared, self()})
    :ok
  end

  describe "joining user_queries" do
    setup [:with_user, :with_socket]

    test "allows joining own channel", %{user: user, socket: socket} do
      assert {:ok, _, _} = subscribe_and_join(socket, UserChannel, "user_queries:#{user.id}")
    end

    test "can't join another's channel", %{user: user, socket: socket} do
      assert {:error, _} = subscribe_and_join(socket, UserChannel, "user_queries:#{user.id + 1}")
    end
  end

  describe "notifications on user_queries" do
    setup [:with_user, :with_socket, :subscribed_to_user_queries]

    test "receiving state updates", %{user: user} do
      query = create_query!(user, %{data_source_id: create_data_source!().id})

      UserChannel.broadcast_state_change(query)

      expected = Air.Schemas.Query.for_display(query)
      assert_push("state_change", ^expected)
    end

    test "no state updates for queries belonging to other users" do
      query = create_query!(_other_user = create_user!(), %{data_source_id: create_data_source!().id})

      UserChannel.broadcast_state_change(query)

      refute_push("state_change", _)
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
      query = create_query!(some_user, %{query_state: :started, data_source_id: data_source.id})
      query_id = query.id

      UserChannel.broadcast_state_change(query)

      assert_push("state_change", %{query_id: ^query_id, event: :started, query: %{id: ^query_id}})
    end
  end

  defp with_user(_), do: {:ok, user: create_user!()}

  defp made_admin(context), do: {:ok, user: make_admin!(context[:user])}

  defp with_socket(context), do: {:ok, socket: socket("user", %{user: context[:user]})}

  defp subscribed_to_all_state_changes(context) do
    {:ok, _, _} = subscribe_and_join(context[:socket], UserChannel, "state_changes:all")
    :ok
  end

  defp subscribed_to_user_queries(context) do
    {:ok, _, _} = subscribe_and_join(context.socket, UserChannel, "user_queries:#{context.user.id}")
    :ok
  end
end

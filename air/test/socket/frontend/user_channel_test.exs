defmodule Air.Socket.Frontend.UserChannelTest do
  alias Air.Socket.Frontend.UserChannel

  import Air.{TestRepoHelper}

  use Air.ChannelCase, async: true

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Air.Repo)
  end

  setup do
    user = create_user!()
    {:ok, _, _} =
      socket("user", %{user: user})
      |> subscribe_and_join(UserChannel, "user:" <> to_string(user.id))

    {:ok, user: user}
  end

  test "results of queries are pushed to the user", %{user: user} do
    query = create_query!(user)

    UserChannel.broadcast_result(query)

    expected = Air.Query.for_display(query)

    assert_push("result", ^expected)
  end

  test "results of other user's queries are not pushed to the user" do
    query = create_query!(_other_user = create_user!())
    result = Air.Query.for_display(query)

    UserChannel.broadcast_result(query)

    refute_push("result", ^result)
  end
end

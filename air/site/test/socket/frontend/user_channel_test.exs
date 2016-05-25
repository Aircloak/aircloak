defmodule Air.Socket.Frontend.UserChannelTest do
  alias Air.Socket.Frontend.UserChannel

  import Air.{TestRepoHelper}

  use Air.ChannelCase

  setup do
    user = create_user!()
    {:ok, _, _} =
      socket("user", %{user: user})
      |> subscribe_and_join(UserChannel, "user:" <> to_string(user.id))

    {:ok, user: user}
  end

  test "results of tasks are pushed to the user", %{user: user} do
    task = create_task!(user)
    result = %{"query_id" => task.id, "some" => "data"}

    UserChannel.broadcast_result(result)

    expected = Map.put(result, "query", task.query)
    assert_push("result", ^expected)
  end

  test "results of other user's tasks are not pushed to the user" do
    task = create_task!(_other_user = create_user!())
    result = %{"query_id" => task.id, "some" => "data"}

    UserChannel.broadcast_result(result)

    refute_push("result", ^result)
  end
end

defmodule AirWeb.Socket.Frontend.DataSourceChannel.Test do
  alias AirWeb.Socket.Frontend.DataSourceChannel

  import Air.TestRepoHelper

  use AirWeb.ChannelCase, async: false

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Air.Repo)
    Ecto.Adapters.SQL.Sandbox.mode(Air.Repo, {:shared, self()})
    :ok
  end

  describe "joining a data source channel" do
    setup [:with_user, :with_socket, :with_data_source]

    test "allows joining when user can see data source", %{
      user: user,
      socket: socket,
      data_source: data_source
    } do
      assign_data_source_to_user(data_source, user)

      assert {:ok, _, _} = subscribe_and_join(socket, DataSourceChannel, "data_source:#{data_source.name}")
    end

    test "can't join when user can't see data source", %{socket: socket, data_source: data_source} do
      assert {:error, _} = subscribe_and_join(socket, DataSourceChannel, "data_source:#{data_source.id}")
    end
  end

  describe "pushing updates" do
    setup [:with_user, :with_socket, :with_data_source, :joined_channel]

    test "pushes an offline message for offline data sources" do
      DataSourceChannel.push_updates()

      assert_push("status", %{status: :offline})
    end

    test "pushes an online message for online data sources", %{data_source: data_source} do
      register_data_source!(data_source)
      DataSourceChannel.push_updates()

      assert_push("status", %{status: :online})
    end
  end

  defp with_user(_), do: {:ok, user: create_user!()}

  defp with_socket(context), do: {:ok, socket: socket("user", %{user: context[:user]})}

  defp with_data_source(_), do: {:ok, data_source: create_data_source!()}

  defp joined_channel(%{user: user, socket: socket, data_source: data_source}) do
    assign_data_source_to_user(data_source, user)

    {:ok, _, socket} = subscribe_and_join(socket, DataSourceChannel, "data_source:#{data_source.name}")

    {:ok, socket: socket}
  end

  defp assign_data_source_to_user(data_source, user),
    do: create_group!(%{users: [user.id], data_sources: [data_source.id]})
end

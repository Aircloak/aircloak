defmodule CentralWeb.Socket.AirTest do
  # `async: false` because shared sandbox mode is used
  # (see https://hexdocs.pm/ecto/Ecto.Adapters.SQL.Sandbox.html)
  use CentralWeb.ChannelCase, async: false

  alias Phoenix.Channels.GenSocketClient
  alias GenSocketClient.TestSocket
  alias Central.{TestSocketHelper, Repo}
  alias Central.Service.Customer
  alias Central.Schemas.Query

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
    Ecto.Adapters.SQL.Sandbox.mode(Repo, {:shared, self()})
    :ok
  end

  test "air name must be provided" do
    params = %{air_name: ""}
    assert {{:disconnected, {403, "Forbidden"}}, _} = TestSocketHelper.connect(params)
  end

  test "unmatched topic" do
    socket = connect!()
    assert {:error, reason} = TestSocket.join(socket, "invalid_channel")
    assert {:server_rejected, "invalid_channel", %{"reason" => "unmatched topic"}} == reason
  end

  test "main topic" do
    socket = connect!()
    assert {:ok, %{}} == join_main_channel(socket)
  end

  # These tests are used to lock down the API for a given air version. Unless there is a bug they should probably not
  # be changed to ensure older airs can still communicate with central. Separate handling for newer airs should be
  # introduced instead along with appropriate tests. They can be removed once a given air version is no longer
  # supported.
  for join_options <- [%{}, %{"air_version" => "18.1.0"}, %{"air_version" => "18.2.0"}] do
    @join_options join_options

    describe "main topic with #{inspect join_options}" do
      setup do: {:ok, join_options: @join_options}

      setup [:with_customer, :joined_main]

      test "query_execution", %{socket: socket, customer: customer} do
        request_id = push_air_call(socket, "query_execution", %{
          metrics: %{"some" => "metrics"},
          features: %{"some" => "features"},
          aux: %{"some" => "data"},
        })

        assert_push "central_response", %{request_id: ^request_id, status: :ok}
        assert %{
          metrics: %{"some" => "metrics"}, features: %{"some" => "features"}, aux: %{"some" => "data"}
        } = Repo.get_by(Query, customer_id: customer.id)
      end

      test "a duplicate message", %{socket: socket} do
        message_id = Ecto.UUID.generate()
        request_id = Ecto.UUID.generate()
        message = %{
          metrics: %{"some" => "metrics"},
          features: %{"some" => "features"},
          aux: %{"some" => "data"},
        }

        assert Enum.empty?(Repo.all(Query)), "No queries recorded initially"

        push_air_call(socket, "query_execution", message, message_id, request_id)
        push_air_call(socket, "query_execution", message, message_id, request_id)
        push_air_call(socket, "query_execution", message, message_id, request_id)

        assert 1 == length(Repo.all(Query)), "Repeated messages are dropped"
      end
    end
  end

  defp connect!(params \\ %{}) do
    TestSocketHelper.connect!(Map.merge(default_params(), params))
  end

  defp join_main_channel(socket) do
    TestSocketHelper.join!(socket, "main", %{})
  end

  defp default_params() do
    {:ok, customer} = Customer.create(%{name: "test customer"})
    {:ok, token} = Customer.generate_token(customer)
    %{air_name: "air_1", token: token}
  end

  defp with_customer(_context) do
    {:ok, customer} = Customer.create(%{name: "test customer"})
    {:ok, customer: customer}
  end

  defp joined_main(%{join_options: join_options, customer: customer}) do
    {:ok, token} = Customer.generate_token(customer)
    {:ok, socket} = Phoenix.ChannelTest.connect(CentralWeb.Socket.Air, %{token: token, air_name: "air_name"})
    {:ok, _, socket} = Phoenix.ChannelTest.subscribe_and_join(socket, "main", join_options)
    {:ok, socket: socket}
  end

  defp push_air_call(socket, event, payload, message_id \\ Ecto.UUID.generate(), request_id \\ Ecto.UUID.generate()) do
    push(socket, "air_call", %{request_id: request_id, event: "call_with_retry", payload: %{
      id: message_id, event: event, payload: payload}})
    :timer.sleep(50)

    request_id
  end
end

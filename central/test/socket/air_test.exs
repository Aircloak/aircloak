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
  for join_options <- [%{}, %{"air_version" => "17.1.0"}, %{"air_version" => "17.2.0"}] do
    @join_options join_options

    describe "main topic with #{inspect join_options}" do
      setup do: {:ok, join_options: @join_options}

      setup [:with_customer, :with_air, :joined_main]

      test "cloak_online", %{air: air, socket: socket} do
        cloak_name = Ecto.UUID.generate()
        request_id = push_air_call(socket, "cloak_online", %{
          name: cloak_name,
          data_source_names: ["ds1", "ds2"],
          version: "129",
        })

        assert_push "central_response", %{request_id: ^request_id, status: :ok}
        assert %{
          data_source_names: ["ds1", "ds2"], version: "129", status: :online
        } = Repo.get_by(Cloak, name: cloak_name, air_id: air.id)
      end

      test "cloak_offline", %{air: air, socket: socket} do
        cloak_name = Ecto.UUID.generate()
        request_id = push_air_call(socket, "cloak_offline", %{name: cloak_name})

        assert_push "central_response", %{request_id: ^request_id, status: :ok}
        assert %{status: :offline} = Repo.get_by(Cloak, name: cloak_name, air_id: air.id)
      end

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

      test "usage_info", %{socket: socket, customer: customer} do
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

      test "a duplicate message", %{socket: socket, air: air} do
        cloak_name = Ecto.UUID.generate()
        message_id = Ecto.UUID.generate()

        push_air_call(socket, "cloak_online", %{name: cloak_name, data_source_names: [], version: "129"}, message_id)
        push_air_call(socket, "cloak_offline", %{name: cloak_name})
        push_air_call(socket, "cloak_online", %{name: cloak_name, data_source_names: [], version: "129"}, message_id)

        assert %{status: :offline} = Repo.get_by(Cloak, name: cloak_name, air_id: air.id)
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

  defp with_air(%{customer: customer}), do:
    {:ok, air: %Air{name: "air_1", customer_id: customer.id, status: :online} |> Repo.insert!()}

  defp joined_main(%{join_options: join_options, customer: customer, air: air}) do
    {:ok, token} = Customer.generate_token(customer)
    {:ok, socket} = Phoenix.ChannelTest.connect(CentralWeb.Socket.Air, %{token: token, air_name: air.name})
    {:ok, _, socket} = Phoenix.ChannelTest.subscribe_and_join(socket, "main", join_options)
    {:ok, socket: socket}
  end

  defp push_air_call(socket, event, payload, message_id \\ Ecto.UUID.generate()) do
    request_id = Ecto.UUID.generate()

    push(socket, "air_call", %{request_id: request_id, event: "call_with_retry", payload: %{
      id: message_id, event: event, payload: payload}})
    :timer.sleep(50)

    request_id
  end
end

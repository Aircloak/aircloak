defmodule Central.Socket.AirTest do
  # `async: false` because shared sandbox mode is used
  # (see https://hexdocs.pm/ecto/Ecto.Adapters.SQL.Sandbox.html)
  use Central.ChannelCase, async: false

  alias Phoenix.Channels.GenSocketClient
  alias GenSocketClient.TestSocket
  alias Central.{TestSocketHelper, Repo}
  alias Central.Service.Customer
  alias Central.Schemas.{Air, Cloak, Query}

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
  for version <- ~w(17.1.0 17.2.0) do
    describe "messaging in #{version}" do
      setup do: {:ok, version: unquote(version)}

      setup [:with_customer, :with_air, :joined_main]

      test "cloak_online", %{air: air, socket: socket} do
        cloak_name = Ecto.UUID.generate()
        request_id = push_air_call(socket, "cloak_online", %{
          name: cloak_name,
          data_source_names: ["ds1", "ds2"],
          version: "129",
        })

        assert_push "call_response", %{request_id: ^request_id, status: :ok}
        assert soon(fn() -> match?(
          %{data_source_names: ["ds1", "ds2"], version: "129", status: :online},
          Repo.get_by(Cloak, name: cloak_name, air_id: air.id)
        ) end)
        wait_for_cleanup()
      end

      test "cloak_offline", %{air: air, socket: socket} do
        cloak_name = Ecto.UUID.generate()
        request_id = push_air_call(socket, "cloak_offline", %{name: cloak_name})

        assert_push "call_response", %{request_id: ^request_id, status: :ok}
        assert soon(fn() -> match?(
          %{status: :offline},
          Repo.get_by(Cloak, name: cloak_name, air_id: air.id)
        ) end)
        wait_for_cleanup()
      end

      test "query_execution", %{socket: socket, customer: customer} do
        request_id = push_air_call(socket, "query_execution", %{
          metrics: %{"some" => "metrics"},
          features: %{"some" => "features"},
          aux: %{"some" => "data"},
        })

        assert_push "call_response", %{request_id: ^request_id, status: :ok}
        assert soon(fn() -> match?(
          %{metrics: %{"some" => "metrics"}, features: %{"some" => "features"}, aux: %{"some" => "data"}},
          Repo.get_by(Query, customer_id: customer.id)
        ) end)
        wait_for_cleanup()
      end

      test "usage_info", %{socket: socket, customer: customer} do
        request_id = push_air_call(socket, "query_execution", %{
          metrics: %{"some" => "metrics"},
          features: %{"some" => "features"},
          aux: %{"some" => "data"},
        })

        assert_push "call_response", %{request_id: ^request_id, status: :ok}
        assert soon(fn() -> match?(
          %{metrics: %{"some" => "metrics"}, features: %{"some" => "features"}, aux: %{"some" => "data"}},
          Repo.get_by(Query, customer_id: customer.id)
        ) end)
        wait_for_cleanup()
      end

      test "a duplicate message", %{socket: socket, air: air} do
        cloak_name = Ecto.UUID.generate()
        message_id = Ecto.UUID.generate()

        push_air_call(socket, "cloak_online", %{name: cloak_name, data_source_names: [], version: "129"}, message_id)
        :timer.sleep(50)
        push_air_call(socket, "cloak_offline", %{name: cloak_name})
        :timer.sleep(50)
        push_air_call(socket, "cloak_online", %{name: cloak_name, data_source_names: [], version: "129"}, message_id)

        :timer.sleep(50)
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

  defp joined_main(%{version: version, customer: customer, air: air}) do
    {:ok, token} = Customer.generate_token(customer)
    {:ok, socket} = Phoenix.ChannelTest.connect(Central.Socket.Air, %{token: token, air_name: air.name})
    {:ok, _, socket} = Phoenix.ChannelTest.subscribe_and_join(socket, "main", %{"air_version" => version})
    {:ok, socket: socket}
  end

  defp push_air_call(socket, event, payload, message_id \\ Ecto.UUID.generate()) do
    request_id = Ecto.UUID.generate()

    push(socket, "air_call", %{request_id: request_id, event: "call_with_retry", payload: %{
      id: message_id, event: event, payload: payload}})

    request_id
  end

  defp wait_for_cleanup() do
    # This tries to avoid messages about abandoned connections
    :timer.sleep(10)
  end

  defp soon(check, timeout \\ 100), do:
    perform_soon_check(check, 10, div(timeout, 10))

  defp perform_soon_check(_check, 0, _repeat_wait_time), do: false
  defp perform_soon_check(check, remaining_attempts, repeat_wait_time) do
    if check.() do
      true
    else
      :timer.sleep(repeat_wait_time)
      perform_soon_check(check, remaining_attempts - 1, repeat_wait_time)
    end
  end
end

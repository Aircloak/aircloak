defmodule Cloak.CloakSocketMock do
  defmodule Endpoint do
    use Phoenix.Endpoint, otp_app: :aircloak_common

    socket "/cloak/socket", Cloak.CloakSocketMock.Socket

    defoverridable start_link: 0
    def start_link do
      Application.put_env(:aircloak_common, __MODULE__, [
        https: false,
        http: [port: 29_876],
        secret_key_base: String.duplicate("abcdefgh", 8),
        debug_errors: false,
        server: true,
        pubsub: [adapter: Phoenix.PubSub.PG2, name: __MODULE__]
      ])

      super()
    end
  end

  defmodule Socket do
    @moduledoc false
    use Phoenix.Socket

    transport :websocket, Phoenix.Transports.WebSocket

    # List of exposed channels
    channel "main", Cloak.CloakSocketMock.MainChannel

    def kill(cloak_name) do
      Process.exit(:gproc.where({:n, :l, {:socket, cloak_name}}), :kill)
    end

    def connect(params, socket) do
      cloak_name = params["cloak_name"]
      if cloak_name != nil do
        cloak_name = "#{cloak_name}"
        :gproc.reg({:n, :l, {:socket, cloak_name}})
        {:ok, assign(socket, :cloak_name, cloak_name)}
      else
        :error
      end
    end

    def id(_socket), do: ""
  end

  defmodule MainChannel do
    @moduledoc false
    use Phoenix.Channel

    def await(cloak_name, timeout \\ :timer.seconds(1)) do
      {pid, _} = :gproc.await(reg_name(cloak_name), timeout)
      pid
    end

    def reg_name(cloak_name), do: {:n, :l, {:cloak, cloak_name}}

    def pid(cloak_name), do: :gproc.where(reg_name(cloak_name))

    def subscribe(cloak_name), do: :gproc.reg({:p, :l, pid(cloak_name)})

    def cloak_info(cloak_name) do
      call(cloak_name, :cloak_info)
    end

    def leave(cloak_name) do
      send(pid(cloak_name), :leave)
    end

    def send_to_cloak(cloak_name, event, payload) do
      send(pid(cloak_name), {:send_to_cloak, event, payload})
    end

    defp call(cloak_name, request, timeout \\ 100) do
      ref = make_ref()
      send(pid(cloak_name), {:call, self(), ref, request})
      receive do
        {^ref, response} -> response
      after timeout -> raise "timeout"
      end
    end

    def join("main", cloak_info, socket) do
      :gproc.reg(reg_name(socket.assigns.cloak_name))
      {:ok, %{}, socket |> assign(:cloak_info, cloak_info)}
    end

    def handle_in(event, payload, socket) do
      :gproc.send({:p, :l, self()}, {:in_message, event, payload})
      {:noreply, socket}
    end

    def handle_info({:call, caller, ref, request}, socket) do
      send(caller, {ref, handle_call(request, socket)})
      {:noreply, socket}
    end
    def handle_info(:leave, socket) do
      {:stop, :ok, socket}
    end
    def handle_info({:send_to_cloak, event, payload}, socket) do
      push(socket, event, payload)
      {:noreply, socket}
    end

    def handle_call(:cloak_info, socket) do
      socket.assigns.cloak_info
    end
  end
end

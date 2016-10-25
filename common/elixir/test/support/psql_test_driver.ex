defmodule Aircloak.PsqlTestDriver do
  @moduledoc false

  alias Aircloak.PsqlServer.RanchServer
  @behaviour RanchServer

  defmacro run_client(port, user, pass, query \\ nil, opts) do
    quote do
      test_process_pid = self()

      task = Task.async(fn ->
        # Ugly temp hack to give us some testability. We can't use Postgrex or other Erlang drivers, since
        # they use prepared statements. Once we add support for that, we'll be able to replace `psql` with this
        :os.cmd('
          PGPASSWORD=#{unquote(pass)} psql \\
            -h localhost -p #{unquote(port)} -U #{unquote(user)} -d "#{:erlang.pid_to_list(test_process_pid)}" \\
            #{unquote(if query, do: '-c "#{query}"', else: '')}
        ')
        |> to_string()
      end)

      unquote(opts[:do])

      task
    end
  end

  defmacro handle_server_event(message_pattern, conn_pattern, opts) do
    quote do
      assert_receive({:"$gen_call", from, {unquote(conn_pattern), unquote(message_pattern)}}, :timer.seconds(1))
      response = unquote(Keyword.fetch!(opts, :do))
      GenServer.reply(from, response)
    end
  end

  def listen(port), do:
    RanchServer.start_embedded_server(port, __MODULE__, nil,
      ssl: [
        certfile: "../../air/priv/config/ssl_cert.pem",
        keyfile: "../../air/priv/config/ssl_key.pem"
      ])

  def init(conn, nil), do:
    {:ok, conn}

  def login(conn, password), do:
    conn
    |> RanchServer.assign(:test_pid,
          Map.fetch!(conn.login_params, "database")
          |> to_charlist()
          |> :erlang.list_to_pid()
        )
    |> call({:login, password})

  def run_query(conn, query), do:
    call(conn, {:run_query, query})

  def handle_message(conn, message), do:
    call(conn, {:handle_message, message})

  defp call(conn, message), do:
    GenServer.call(conn.assigns.test_pid, {conn, message})
end

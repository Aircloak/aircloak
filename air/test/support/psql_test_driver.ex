defmodule Air.PsqlTestDriver do
  @moduledoc false

  alias Air.PsqlServer.RanchServer
  @behaviour RanchServer

  defmacro __using__(_opts) do
    quote do
      import unquote(__MODULE__), only: [start_client: 3, start_client: 4, handle_server_event: 3]
    end
  end

  def start_client(port, user, pass, query \\ nil) do
    test_process_pid = self()

    Task.async(fn ->
      connection_string =
        %{
          "DSN" => "PostgreSQL",
          "Server" => "localhost",
          "Port" => port,
          "sslmode" => "require",
          "Uid" => user,
          "Pwd" => pass,
          "Database" => :erlang.pid_to_list(test_process_pid)
        }
        |> Enum.map(fn({name, value}) -> "#{name}=#{value};" end)
        |> Enum.join()
        |> to_charlist()

      with {:ok, conn} <- :odbc.connect(connection_string, []) do
        if query do
          :odbc.sql_query(conn, to_charlist(query))
        else
          :ok
        end
      end
    end)
  end

  defmacro handle_server_event(message_pattern, conn_pattern, opts) do
    quote do
      assert_receive({:"$gen_call", from, {unquote(conn_pattern), unquote(message_pattern)}}, :timer.seconds(2))
      response = unquote(Keyword.fetch!(opts, :do))
      GenServer.reply(from, response)
    end
  end

  def listen(port), do:
    RanchServer.start_embedded_server(port, __MODULE__, nil,
      ssl: [
        certfile: Path.join([Application.app_dir(:air, "priv"), "config", "ssl_cert.pem"]),
        keyfile: Path.join([Application.app_dir(:air, "priv"), "config", "ssl_key.pem"])
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

  def run_query(conn, query) do
    downcased_query = String.downcase(query)
    if match?("set " <> _, downcased_query) or
       downcased_query == "select oid, typbasetype from pg_type where typname = 'lo'" do
      # A few queries always sent by the ODBC driver. We're ignoring them here for now.
      RanchServer.set_query_result(conn, %{columns: [], rows: []})
    else
      call(conn, {:run_query, query})
    end
  end

  def handle_message(conn, message), do:
    call(conn, {:handle_message, message})

  defp call(conn, message) do
    try do
      GenServer.call(conn.assigns.test_pid, {conn, message}, :timer.seconds(2))
    catch type, reason ->
      raise "Error calling test process:\n#{inspect message}\n#{inspect type}:#{inspect reason}"
    end
  end
end

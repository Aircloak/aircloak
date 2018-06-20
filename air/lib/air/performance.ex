defmodule Air.Performance do
  @moduledoc "Module for running a performance comparison between cloak queries and raw database queries."

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Synchronously executes the performance comparison and return observed measurements."
  @spec run(String.t(), String.t(), String.t()) :: :ok
  def run(cloak_datasource_folder, user_name, password) do
    conns = %{
      db: connect_db!(cloak_datasource_folder),
      cloak_unencoded: connect_aircloak!("cloak_performance", user_name, password),
      cloak_encoded: connect_aircloak!("cloak_performance_encoded", user_name, password)
    }

    aircloak_latency = aircloak_latency(conns)
    num_users = num_users(conns)

    result =
      fn ->
        Enum.map(
          Air.Performance.Queries.queries(),
          &measure_query(conns, aircloak_latency, num_users, &1)
        )
      end
      |> Task.async()
      |> Task.await(:timer.hours(10))

    # closing connections in background to avoid possible disconnect error to prevent returning the result
    Enum.each(conns, fn {_key, conn} ->
      Task.async(fn -> GenServer.stop(conn, :normal, :timer.seconds(5)) end)
    end)

    fields = [
      :num_users,
      :db_time,
      :cloak_unencoded_time,
      :cloak_encoded_time,
      :aircloak_latency,
      :statement
    ]

    result
    |> CSV.encode(headers: fields)
    |> Enum.to_list()
    |> IO.puts()
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp aircloak_latency(conns) do
    num_measurements = 10

    1..num_measurements
    |> Stream.map(fn _ ->
      measure_statement(conns.cloak_unencoded, "select count(*) from users where id=-1")
    end)
    # we'll take the smallest observed latency, which leads to the most pessimistic overall results
    |> Enum.min()
  end

  defp num_users(conns) do
    %Postgrex.Result{rows: [[count]]} = Postgrex.query!(conns.db, "select count(*) from users", [])

    count
  end

  defp measure_query(conns, aircloak_latency, num_users, statement) do
    conns
    |> Enum.map(&measure_conn(&1, statement))
    |> Enum.into(%{
      num_users: num_users,
      aircloak_latency: aircloak_latency,
      statement: display_statement(statement)
    })
  end

  defp display_statement(%{cloak: statement}), do: normalize_whitespaces(statement)

  defp display_statement(statement) when is_binary(statement), do: normalize_whitespaces(statement)

  defp normalize_whitespaces(statement) do
    statement
    |> String.trim()
    |> String.replace(~r/\s+/, " ")
  end

  defp measure_conn({key, conn}, statement) do
    statement = parse_statement(key, statement)
    {:"#{key}_time", measure_statement(conn, statement)}
  end

  defp measure_statement(conn, statement) do
    {time, _result} = :timer.tc(fn -> Postgrex.query!(conn, statement, [], timeout: :timer.hours(1)) end)

    :erlang.convert_time_unit(time, :microsecond, :millisecond)
  end

  def parse_statement(_key, statement) when is_binary(statement), do: statement
  def parse_statement(:db, %{} = statement), do: statement.db
  def parse_statement(_key, %{} = statement), do: statement.cloak

  defp connect_aircloak!(data_source_name, user_name, password) do
    await_datasource(data_source_name)

    {:ok, conn} =
      Postgrex.start_link(
        hostname: "localhost",
        port: :air |> Application.fetch_env!(Air.PsqlServer) |> Keyword.fetch!(:port),
        username: user_name,
        password: password,
        database: data_source_name,
        ssl: false
      )

    conn
  end

  defp connect_db!(cloak_datasource_folder) do
    parameters =
      cloak_datasource_folder
      |> Path.join("cloak_performance.json")
      |> File.read!()
      |> Poison.decode!()
      |> Map.fetch!("parameters")
      |> Enum.map(fn {name, value} -> {String.to_atom(name), value} end)
      |> Enum.concat(after_connect: &Postgrex.query!(&1, "set search_path to projections, public", []))

    {:ok, conn} = Postgrex.start_link(parameters)
    conn
  end

  defp await_datasource(data_source_name) do
    IO.puts("Awaiting #{data_source_name}...")
    max_retries = 300
    delay = :timer.seconds(1)

    Stream.repeatedly(fn -> data_source_ready?(data_source_name) end)
    |> Stream.with_index()
    |> Stream.take_while(fn {value, index} -> value != true and index < max_retries end)
    |> Stream.intersperse(:sleep)
    |> Enum.each(fn el -> if el == :sleep, do: Process.sleep(delay) end)

    IO.puts("#{data_source_name} ready")
  end

  defp data_source_ready?(data_source_name) do
    case Air.Service.DataSource.all() |> Enum.find(&(&1.name == data_source_name)) do
      nil -> false
      data_source -> Air.Service.DataSource.status(data_source) == :online
    end
  end
end

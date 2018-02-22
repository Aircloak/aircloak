defmodule Air.Performance do
  @moduledoc "Module for running a performance comparison between cloak queries and raw database queries."

  @type result :: %{
    statement: String.t,
    db_time: non_neg_integer,
    unencoded_time: non_neg_integer,
    encoded_time: non_neg_integer,
  }

  @test_queries [
    "select name, count(*) from users group by name"
  ]


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Synchronously executes the performance comparison and return observed measurements."
  @spec run(String.t, String.t, String.t) :: [result]
  def run(cloak_datasource_folder, user_name, password) do
    conns =
      %{
        db: connect_db!(cloak_datasource_folder),
        unencoded: connect_aircloak!("cloak_performance", user_name, password),
        encoded: connect_aircloak!("cloak_performance_encoded", user_name, password),
      }

    result =
      fn -> Enum.map(@test_queries, &measure_query(conns, &1)) end
      |> Task.async()
      |> Task.await(:timer.hours(10))

    # closing connections in background to avoid possible disconnect error to prevent returning the result
    Enum.each(
      conns,
      fn({_key, conn}) -> Task.async(fn -> GenServer.stop(conn, :normal, :timer.seconds(5)) end) end
    )

    result
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp measure_query(conns, statement) do
    conns
    |> Enum.map(&measure_conn(&1, statement))
    |> Enum.into(%{statement: statement})
  end

  defp measure_conn({key, conn}, statement) do
    {time, _result} = :timer.tc(fn -> Postgrex.query(conn, statement, [], timeout: :timer.hours(1)) end)
    {:"#{key}_time", :erlang.convert_time_unit(time, :microsecond, :millisecond)}
  end

  defp connect_aircloak!(data_source_name, user_name, password) do
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
      |> Enum.map(fn({name, value}) -> {String.to_atom(name), value} end)

    {:ok, conn} = Postgrex.start_link(parameters)
    conn
  end
end

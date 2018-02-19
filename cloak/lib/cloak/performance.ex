defmodule Cloak.Performance do
  @moduledoc """
  Server process which compares query performance between cloak and raw database.

  This functionality is implemented behind a named GenServer to ensure that at any point in time we're running at most
  one performance measurement.
  """

  use GenServer
  require Logger
  alias Cloak.DataSource.PostgreSQL

  @type result :: %{
    statement: String.t,
    db_time: non_neg_integer,
    plain_cloak_time: non_neg_integer,
    encoded_cloak_time: non_neg_integer,
  }

  @test_queries [
    "select age, count(*) from users group by age"
  ]


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Measures execution time of various queries on raw database and cloak (plain and encoded)."
  @spec run() :: [result]
  def run(), do: GenServer.call(__MODULE__, :run, :timer.hours(1))


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(_), do: {:ok, nil}

  @impl GenServer
  def handle_call(:run, _from, state) do
    result = Enum.map(@test_queries, &measure_query/1)
    {:reply, result, state}
  end

  @impl GenServer
  def handle_info({:state, _}, state), do: {:noreply, state}
  def handle_info(other, state), do: super(other, state)


  # -------------------------------------------------------------------
  # Private functions
  # -------------------------------------------------------------------

  defp measure_query(statement) do
    {db_time, _} = measure(fn -> db_query(statement) end)
    {plain_cloak_time, _} = measure(fn -> cloak_query(data_source("cloak_performance"), statement) end)
    {encoded_cloak_time, _} = measure(fn -> cloak_query(data_source("cloak_performance_encoded"), statement) end)

    %{
      statement: statement,
      db_time: db_time,
      plain_cloak_time: plain_cloak_time,
      encoded_cloak_time: encoded_cloak_time,
    }
  end

  defp db_query(statement) do
    conn = PostgreSQL.connect!(data_source("cloak_performance").parameters)
    Postgrex.query!(conn, statement, [])
  end

  defp cloak_query(data_source, statement) do
    :crypto.strong_rand_bytes(10)
    |> Base.encode64(padding: false)
    |> Cloak.Query.Runner.run_sync(data_source, statement, [], %{})
  end

  defp data_source(name), do: Enum.find(Cloak.DataSource.all(), &(&1.name == name))

  defp measure(fun) do
    {time, result} = :timer.tc(fun)
    {:erlang.convert_time_unit(time, :microsecond, :millisecond), result}
  end


  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(_arg), do: GenServer.start_link(__MODULE__, nil, name: __MODULE__)
end

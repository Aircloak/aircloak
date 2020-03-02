defmodule Air.Service.Cloak.AnalysisCache do
  @moduledoc """
  Holds and processes Analysis results that are dispatched between Cloak instances.
  """
  use GenServer

  @type analysis :: %{
          descriptor: binary,
          result: binary,
          type: any,
          status: :ok,
          expires: NaiveDateTime.t()
        }

  @doc false
  def start_link(_) do
    GenServer.start_link(__MODULE__, nil, name: __MODULE__)
  end

  @doc "Retrieves all available analysis results."
  @spec all() :: [analysis]
  def all() do
    GenServer.call(__MODULE__, :all)
  end

  @doc "Insert an analysis result into the cache."
  @spec insert(analysis) :: :ok
  def insert(analyses),
    do: GenServer.cast(__MODULE__, {:insert, analyses})

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl true
  def init(nil) do
    {:ok, %{}}
  end

  @impl true
  def handle_call(:all, _from, state) do
    result =
      Enum.flat_map(state, fn {descriptor, results} ->
        Enum.map(results, fn {type, {status, result, expires}} ->
          %{descriptor: descriptor, result: result, type: type, status: status, expires: expires}
        end)
      end)

    {:reply, result, state}
  end

  @impl true
  def handle_cast({:insert, analyses}, state) do
    {:noreply, Enum.reduce(analyses, state, &insert_analysis/2)}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp insert_analysis(analysis, state) do
    put_in(
      state,
      [Access.key(analysis.descriptor, %{}), Access.key(analysis.type, %{})],
      {analysis.status, analysis.result, analysis.expires}
    )
  end
end

defmodule Air.Service.Cloak.Analysis do
  use GenServer

  def start_link(_) do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  def available(_data_sources) do
    # The plan is to use data_sources to filter the amount of data being
    # sent over the socket, but I won't bother for now.
    GenServer.call(__MODULE__, :available_analyses)
  end

  def complete(analyses),
    do: GenServer.cast(__MODULE__, {:analyses_complete, analyses})

  # GenServer Callbacks
  # -------------------

  @impl true
  def init(analyses) do
    {:ok, analyses}
  end

  @impl true
  def handle_call(:available_analyses, _from, state) do
    result =
      Enum.reduce(state, [], fn {descriptor, results}, analyses ->
        Enum.reduce(results, analyses, fn {type, {status, result, expires}}, analyses ->
          [%{descriptor: descriptor, result: result, type: type, status: status, expires: expires} | analyses]
        end)
      end)

    {:reply, result, state}
  end

  @impl true
  def handle_cast({:analyses_complete, analyses}, state) do
    {:noreply, Enum.reduce(analyses, state, &insert_analysis/2)}
  end

  defp insert_analysis(analysis, state) do
    put_in(
      state,
      [Access.key(analysis.descriptor, %{}), Access.key(analysis.type, %{})],
      {analysis.status, analysis.result, analysis.expires}
    )
  end
end

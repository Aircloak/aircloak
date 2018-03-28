defmodule Cloak.CyclicGraph do
  @moduledoc "Implements an undirected cyclic graph."

  @opaque t :: :digraph.graph()
  @type vertex :: any

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Creates the new graph.

  The graph is powered by `:digraph`, which in turn uses ETS tables. Therefore,
  you need to make sure to delete the instance manually with `delete/1`, or
  use `with/1`.
  """
  @spec new() :: t
  def new(), do: :digraph.new([:private, :cyclic])

  @doc "Deletes the graph instance."
  @spec delete(t) :: :ok
  def delete(graph) do
    :digraph.delete(graph)
    :ok
  end

  @doc "Creates a graph, invokes the lambda, deletes the graph, and returns the result of the lambda."
  @spec with((t -> result)) :: result when result: var
  def with(fun) do
    graph = new()
    try do: fun.(graph), after: delete(graph)
  end

  @doc "Adds a vertex to the graph."
  @spec add_vertex(t, vertex) :: :ok
  def add_vertex(graph, vertex) do
    :digraph.add_vertex(graph, vertex)
    :ok
  end

  @doc """
  Connects two existing vertices in the graph.

  This function will raise an error if any of the given vertices doesn't exist in the graph.
  """
  @spec connect!(t, vertex, vertex) :: :ok
  def connect!(graph, vertex1, vertex2) do
    false = match?({:error, _}, :digraph.add_edge(graph, vertex1, vertex2))
    false = match?({:error, _}, :digraph.add_edge(graph, vertex2, vertex1))
    :ok
  end

  @doc """
  Returns all pairs of vertices which are not connected in any way.

  Vertices are not connected if there exist no path which leads from one vertex
  to another.

  The returned list is deduplicated, meaning that each pair of vertices can occur
  at most once in the result.

  This function will always return the same result for the same graph, regardless
  of the order in which vertices and connections are added. To ensure this,
  property, the resulting list is sorted. In the result `[{v1, v2}, {v3, v4}]`,
  the following properties always hold:

    - `v1 < v2`
    - `v3 < v4`
    - `{v1, v2}` < `{v3, v4}`
  """
  @spec disconnected_pairs(t) :: [{vertex, vertex}]
  def disconnected_pairs(graph),
    do:
      :digraph.vertices(graph)
      |> Enum.sort()
      |> pair_combinations()
      |> Enum.filter(fn {v1, v2} -> :digraph.get_path(graph, v1, v2) == false end)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp pair_combinations([]), do: []
  defp pair_combinations([_]), do: []

  defp pair_combinations([el | rest]), do: Stream.concat(Stream.map(rest, &{el, &1}), pair_combinations(rest))
end

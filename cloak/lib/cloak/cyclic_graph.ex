defmodule Cloak.CyclicGraph do
  @moduledoc "Implements an undirected cyclic graph."

  @opaque t :: :digraph.graph
  @type vertex :: any


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Creates the new graph.

  The graph is powered by `:digraph`, which in turn uses ETS tables. Therefore,
  you need to make sure to delete the instance manually with `delete/1`.
  """
  @spec new() :: t
  def new(), do:
    :digraph.new([:private, :cyclic])

  @doc "Deletes the graph instance."
  @spec delete(t) :: :ok
  def delete(graph) do
    :digraph.delete(graph)
    :ok
  end

  @doc "Adds a vertex to the graph."
  @spec add_vertex(t, vertex) :: :ok
  def add_vertex(graph, vertex) do
    ^vertex = :digraph.add_vertex(graph, vertex)
    :ok
  end

  @doc "Connects two existing vertices in the graph."
  @spec connect(t, vertex, vertex) :: :ok
  def connect(graph, vertex1, vertex2) do
    false = match?({:error, _}, :digraph.add_edge(graph, vertex1, vertex2))
    false = match?({:error, _}, :digraph.add_edge(graph, vertex2, vertex1))
    :ok
  end

  @doc """
  Returns all pairs of vertices which are not connected in any way.

  Vertices are not connected if there exist no path which leads from one vertex
  to another. The returned list is deduplicated, meaning that each pair of
  vertices can occur at most once in the result.
  """
  @spec disconnected_pairs(t) :: [{vertex, vertex}]
  def disconnected_pairs(graph), do:
    :digraph.vertices(graph)
    |> pair_combinations()
    |> Enum.filter(fn({v1, v2}) -> :digraph.get_path(graph, v1, v2) == false end)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp pair_combinations([]), do: []
  defp pair_combinations([_]), do: []
  defp pair_combinations([el | rest]), do:
    Stream.concat(Stream.map(rest, &{el, &1}), pair_combinations(rest))
end

defmodule Cloak.CyclicGraphTest do
  use ExUnit.Case, async: true

  alias Cloak.CyclicGraph

  test "empty graph", do: assert([] == CyclicGraph.disconnected_pairs(graph([])))

  test "graph with a single element", do: assert([] == CyclicGraph.disconnected_pairs(graph([:a])))

  test "graph with two disconnected elements", do: assert([a: :b] == CyclicGraph.disconnected_pairs(graph([:a, :b])))

  test "graph with two connected elements", do: assert([] == CyclicGraph.disconnected_pairs(graph([:a, :b], a: :b)))

  test "indirect connections are accounted for",
    do: assert([] == CyclicGraph.disconnected_pairs(graph([:a, :b, :c], a: :b, b: :c)))

  test "returns all disconnected pairs",
    do: assert([a: :c, a: :d, b: :c, b: :d] == CyclicGraph.disconnected_pairs(graph([:a, :b, :c, :d], a: :b, c: :d)))

  test "with" do
    assert [a: :c, b: :c] ==
             CyclicGraph.with(fn graph ->
               CyclicGraph.add_vertex(graph, :a)
               CyclicGraph.add_vertex(graph, :b)
               CyclicGraph.add_vertex(graph, :c)
               CyclicGraph.connect!(graph, :a, :b)
               CyclicGraph.disconnected_pairs(graph)
             end)
  end

  defp graph(vertices, connections \\ []) do
    graph = CyclicGraph.new()
    Enum.each(vertices, &CyclicGraph.add_vertex(graph, &1))
    Enum.each(connections, fn {v1, v2} -> CyclicGraph.connect!(graph, v1, v2) end)
    graph
  end
end

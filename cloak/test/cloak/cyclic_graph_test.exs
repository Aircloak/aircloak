defmodule Cloak.CyclicGraphTest do
  use ExUnit.Case, async: true

  alias Cloak.CyclicGraph

  test "empty graph", do:
    assert [] == disconnected_pairs(graph([]))

  test "graph with a single element", do:
    assert [] == disconnected_pairs(graph([:a]))

  test "graph with two disconnected elements", do:
    assert [a: :b] == disconnected_pairs(graph([:a, :b]))

  test "graph with two connected elements", do:
    assert [] == disconnected_pairs(graph([:a, :b], [a: :b]))

  test "indirect connections are accounted for", do:
    assert [] == disconnected_pairs(graph([:a, :b, :c], [a: :b, b: :c]))

  test "returns all disconnected pairs", do:
    assert [a: :c, a: :d, b: :c, b: :d] == disconnected_pairs(graph([:a, :b, :c, :d], [a: :b, c: :d]))

  defp graph(vertices, connections \\ []) do
    graph = CyclicGraph.new()
    Enum.each(vertices, &CyclicGraph.add_vertex(graph, &1))
    Enum.each(connections, fn({v1, v2}) -> CyclicGraph.connect(graph, v1, v2) end)
    graph
  end

  defp disconnected_pairs(graph), do:
    graph
    |> CyclicGraph.disconnected_pairs()
    |> Enum.map(&(&1 |> Tuple.to_list() |> Enum.sort() |> List.to_tuple()))
    |> Enum.sort()
end

defmodule Air.Schemas.QueryTest do
  use ExUnit.Case, async: true

  alias Air.Schemas.Query

  test "for_display of a finished query" do
    assert %{completed: true} = Query.for_display(%Query{result: %{}})
  end

  test "for_display of an unfinished query" do
    assert %{completed: false} = Query.for_display(%Query{result: nil})
  end

  test "for_display includes all data from result" do
    query = %Query{result: %{"some" => "data"}}
    assert %{"some" => "data"} = Query.for_display(query)
  end
end

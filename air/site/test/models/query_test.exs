defmodule Air.Query.Test do
  use ExUnit.Case, async: true

  alias Air.Query

  test "for_display of a finished query" do
    assert %{completed: true} = Query.for_display(%Query{result: "{}"})
  end

  test "for_display of an unfinished query" do
    assert %{completed: false} = Query.for_display(%Query{result: nil})
  end

  test "for_display of an errored query"

  test "for_display includes rows and columns from result"
end

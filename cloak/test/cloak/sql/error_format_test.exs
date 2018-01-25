defmodule Cloak.Sql.ErrorFormat.Test do
  use ExUnit.Case, async: true

  alias Cloak.Sql.ErrorFormat

  test "returns the simple message if there is no source location", do:
    assert "something" = ErrorFormat.format("ignored", %{message: "something"})

  test "returns the simple message if the source location is nil", do:
    assert "something" = ErrorFormat.format("ignored", %{message: "something", source_location: nil})

  test "adds info about location if the source location is present", do:
    assert ErrorFormat.format("the query", %{message: "Some message.", source_location: {5, 6}}) =~
      ~r/Some message.\n\nThe error was detected at line 5, column 6./

  describe "ASCII-art pointer to error location" do
    test "one-line query" do
      error = ErrorFormat.format("the query", %{message: "Some message.", source_location: {1, 3}})
      assert String.contains?(error, """
      the query
         ^ HERE
      """ |> String.trim())
    end

    test "two-line query"

    test "multi-line query"

    test "location points beyond line end"

    test "location points beyond last line"
  end
end

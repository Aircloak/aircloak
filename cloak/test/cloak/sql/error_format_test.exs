defmodule Cloak.Sql.ErrorFormat.Test do
  use ExUnit.Case, async: true

  alias Cloak.Sql.ErrorFormat

  test "returns the simple message if there is no source location",
    do: assert("something" = ErrorFormat.format("ignored", %{message: "something"}))

  test "returns the simple message if the source location is nil",
    do:
      assert(
        "something" = ErrorFormat.format("ignored", %{message: "something", source_location: nil})
      )

  test "adds info about location if the source location is present",
    do:
      assert(
        ErrorFormat.format("the query", %{message: "Some message.", source_location: {5, 6}}) =~
          ~r/Some message.\n\nThe error was detected at line 5, column 6./
      )

  describe "ASCII-art pointer to error location" do
    test "one-line query" do
      error =
        ErrorFormat.format("the query", %{message: "Some message.", source_location: {1, 3}})

      assert String.contains?(
               error,
               """
               \t1:    the query
               \t        ^
               """
               |> String.trim()
             )
    end

    test "error in first line of multiline query" do
      error =
        ErrorFormat.format("the query\nanother line\nmore lines", %{
          message: "Some message.",
          source_location: {1, 3}
        })

      assert String.contains?(
               error,
               """
               \t1:    the query
               \t        ^
               \t2:    another line
               """
               |> String.trim()
             )
    end

    test "error in second line of two-line query" do
      error =
        ErrorFormat.format("the query\nanother line", %{
          message: "Some message.",
          source_location: {2, 3}
        })

      assert String.contains?(
               error,
               """
               \t1:    the query
               \t2:    another line
               \t        ^
               """
               |> String.trim()
             )
    end

    test "error in middle line of multiline query" do
      error =
        ErrorFormat.format("first line\nsecond line\nthird line\nanother line\nmore lines", %{
          message: "Some message.",
          source_location: {3, 5}
        })

      assert String.contains?(
               error,
               """
               \t2:    second line
               \t3:    third line
               \t          ^
               \t4:    another line
               """
               |> String.trim()
             )
    end

    test "error in the last line of a multiline query" do
      error =
        ErrorFormat.format("first line\nsecond line\nthird line\nanother line\nlast line", %{
          message: "Some message.",
          source_location: {5, 5}
        })

      assert String.contains?(
               error,
               """
               \t4:    another line
               \t5:    last line
               \t          ^
               """
               |> String.trim()
             )
    end

    test "location points beyond last line" do
      error = ErrorFormat.format("one line", %{message: "Some message.", source_location: {5, 5}})
      assert String.contains?(error, "The error was detected at line 5, column 5.")
      refute String.contains?(error, "^")
    end
  end
end

defmodule Air.Service.RedacterTest do
  use Air.SchemaCase, async: true

  alias Air.Service.Redacter

  test "should remove any number of sensitive content", do:
    refute Redacter.filter_query_error("`sensitive1`, `sensitive2`, `sensitive3`") =~ ~r/sensitive/

  test "should replace the sensitve content with a placeholder", do:
    assert Redacter.filter_query_error("`sensitive1`, `sensitive2`, `sensitive3`") =~ ~r/redacted/

  test "should leave non-sensitive information unaltered", do:
    assert Redacter.filter_query_error("this isn't sensitive `this is`") =~ ~r/this isn't sensitive /

  test "should not be greedy in redacting" do
    redacted = Redacter.filter_query_error("Column `bar` doesn't exist in table `players`.")
    assert redacted == "Column `redacted` doesn't exist in table `redacted`."
  end

  test "should support whitelisting" do
    error = "Expected `select or show` at"
    assert error == Redacter.filter_query_error(error)
  end

  test "should replace line and column references" do
    error = "Expected `select or show` at line 1, column 1."
    assert Redacter.filter_query_error(error) =~ ~r/at line X, column Y/
  end

  test "should replace the query excerpt" do
    error = "The error was detected at line 1, column 2:\n\t1:  select *\n\t2: from table\n\t      ^"

    refute Redacter.filter_query_error(error) =~ ~r/1:/
    refute Redacter.filter_query_error(error) =~ ~r/2:/
    refute Redacter.filter_query_error(error) =~ ~r/select \*/
    refute Redacter.filter_query_error(error) =~ ~r/from table/
    refute Redacter.filter_query_error(error) =~ ~r/\^/
  end
end

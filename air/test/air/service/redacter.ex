defmodule Air.Service.RedacterTest do
  use Air.SchemaCase, async: true

  alias Air.Service.Redacter

  test "should remove any number of sensitive content", do:
    refute Redacter.filter_query_error("`sensitive1`, `sensitive2`, `sensitive3`") =~ ~r/sensitive/

  test "should replace the sensitve content with a placeholder", do:
    assert Redacter.filter_query_error("`sensitive1`, `sensitive2`, `sensitive3`") =~ ~r/redacted/

  test "should leave non-sensitive information unaltered", do:
    assert Redacter.filter_query_error("this isn't sensitive `this is`") =~ ~r/this isn't sensitive /
end

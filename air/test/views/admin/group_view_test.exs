defmodule AirWeb.Admin.GroupViewTest do
  use AirWeb.ConnCase, async: true

  import AirWeb.Admin.GroupView

  test "shortes names down to 10 characters" do
    assert String.length(shorten_name("0123456789012")) == 10
  end

  test "leaves short names untouched" do
    assert shorten_name("0123456789") == "0123456789"
  end

  test "long names are shortened in the middle" do
    assert shorten_name("much and too long") == "much a..ng"
  end

  test "when shortened, the individual parts are trimmed too" do
    assert shorten_name("taste the difference") == "taste..ce"
  end

  Enum.each(
    [
      {["a"], "a"},
      {["a", "b"], "a, b"},
      {["a", "b", "c"], "a, b, and c"},
      {["a", "b", "c", "d"], "a, b, and 2 other groups"},
      {["a", "b", "c", "d", "e"], "a, b, and 3 other groups"}
    ],
    fn {names, expected} ->
      test "Formats group of names (#{inspect(names)}) to #{expected}" do
        assert format_names(unquote(names)) === unquote(expected)
      end
    end
  )
end

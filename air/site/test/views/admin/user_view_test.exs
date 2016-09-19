defmodule Air.Admin.UserViewTest do
  use Air.ConnCase, async: true

  import Air.Admin.UserView

  test "shortes names down to 10 characters" do
    assert String.length(shorten_name("0123456789012")) == 10
  end

  test "leaves short names untouched" do
    assert shorten_name("0123456789") == "0123456789"
  end

  test "shortened names are trimmed too" do
    assert shorten_name("namert hats") == "namert..."
  end
end

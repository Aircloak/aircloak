defmodule Air.PageControllerTest do
  use Air.ConnCase, async: true

  test "GET / redirects for auth", %{conn: conn} do
    conn = get conn, "/"
    assert redirected_to(conn) =~ "/auth"
  end
end

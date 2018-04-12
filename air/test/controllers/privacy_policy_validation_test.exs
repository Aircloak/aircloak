defmodule AirWeb.PrivacyPolicy.Validation.Test do
  use AirWeb.ConnCase, async: true

  import Air.{TestConnHelper, TestRepoHelper}

  test "if no privacy policy exists a warning is shown once logged in" do
    user = create_user!()
    logged_in_conn = build_conn() |> post("/auth", email: user.email, password: "1234")
    assert "/" == redirected_to(logged_in_conn)

    # verify that the user can now access a page requiring authentication
    assert recycle(logged_in_conn) |> get("/data_sources") |> response(412)
  end

  test "if no privacy policy exists an admin user is redirected to create one" do
    user = create_admin_user!()
    logged_in_conn = build_conn() |> post("/auth", email: user.email, password: "1234")
    assert "/" == redirected_to(logged_in_conn)

    # verify that the user can now access a page requiring authentication
    assert "/admin/privacy_policy/new" == recycle(logged_in_conn) |> get("/data_sources") |> redirected_to()
  end

  test "no out of the ordinary behaviour if a privacy policy exists" do
    create_privacy_policy!()
    user = create_user!()
    logged_in_conn = build_conn() |> post("/auth", email: user.email, password: "1234")
    assert "/" == redirected_to(logged_in_conn)

    assert get_flash(logged_in_conn)["info"] =~ "Logged in successfully"
    # verify that the user can now access a page requiring authentication
    recycle(logged_in_conn) |> get("/data_sources") |> response(200)
  end

  test "using API without privacy policy yields error", context do
    user = create_user!()
    token = create_token!(user)

    query_data_params = %{
      query: %{statement: "Query code", data_source_name: "some datasource name"}
    }

    assert api_conn(token) |> post("/api/queries", query_data_params) |> response(412)
  end
end

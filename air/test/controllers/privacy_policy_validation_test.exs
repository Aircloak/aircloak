defmodule AirWeb.PrivacyPolicy.Validation.Test do
  use AirWeb.ConnCase, async: false

  import Air.{TestConnHelper, TestRepoHelper}

  test "if no privacy policy exists a warning is shown once logged in" do
    delete_all_privacy_policies!()
    user = create_user_without_privacy_policy!()
    logged_in_conn = build_conn() |> post("/auth", email: user.email, password: "1234")
    assert "/" == redirected_to(logged_in_conn)
    assert recycle(logged_in_conn) |> get("/data_sources") |> response(412)
  end

  test "if no privacy policy exists an admin user is redirected to create one" do
    delete_all_privacy_policies!()
    user = create_user_without_privacy_policy!() |> make_admin!()
    logged_in_conn = build_conn() |> post("/auth", email: user.email, password: "1234")
    assert "/" == redirected_to(logged_in_conn)
    assert "/admin/privacy_policy/new" == recycle(logged_in_conn) |> get("/data_sources") |> redirected_to()
  end

  test "users are asked to accept the privacy policy" do
    create_privacy_policy!()
    user = create_user_without_privacy_policy!()
    logged_in_conn = build_conn() |> post("/auth", email: user.email, password: "1234")
    assert "/" == redirected_to(logged_in_conn)

    assert get_flash(logged_in_conn)["info"] =~ "Logged in successfully"
    privacy_policy_page_html = recycle(logged_in_conn) |> get("/data_sources") |> response(412)

    assert privacy_policy_page_html =~ "review the following privacy policy"
  end

  test "no out of the ordinary behaviour if a privacy policy exists and has been accepted" do
    user = create_user!()
    logged_in_conn = build_conn() |> post("/auth", email: user.email, password: "1234")
    assert "/" == redirected_to(logged_in_conn)

    assert get_flash(logged_in_conn)["info"] =~ "Logged in successfully"
    recycle(logged_in_conn) |> get("/data_sources") |> response(200)
  end

  test "using API without privacy policy yields error" do
    delete_all_privacy_policies!()
    user = create_user_without_privacy_policy!()
    token = create_token!(user)

    query_data_params = %{
      query: %{statement: "Query code", data_source_name: "some datasource name"}
    }

    assert api_conn(token) |> post("/api/queries", query_data_params) |> response(412)
  end

  test "using API without accepted privacy policy yields error" do
    create_privacy_policy!()
    user = create_user_without_privacy_policy!()
    token = create_token!(user)

    query_data_params = %{
      query: %{statement: "Query code", data_source_name: "some datasource name"}
    }

    assert api_conn(token) |> post("/api/queries", query_data_params) |> response(412)
  end
end

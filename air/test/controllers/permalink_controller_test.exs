defmodule AirWeb.PermalinkController.Test do
  use AirWeb.ConnCase, async: true

  import Air.{TestConnHelper, TestRepoHelper}

  setup do
    user = create_user!()
    {:ok, user: user, query: create_query!(user)}
  end

  describe ".query" do
    test "it renders the query for a public token" do
      query = create_query!(create_user!())
      token = Air.Service.Token.public_query_token(query)

      conn = build_conn()
      assert conn |> get(public_permalink_path(conn, :query, token)) |> response(200) =~ query.statement
    end

    test "it renders the query for a private token" do
      query = create_query!(create_user!())

      group = create_group!(%{data_sources: [query.data_source_id]})
      user = create_user!(%{groups: [group.id]})
      token = Air.Service.Token.private_query_token(query)

      assert login(user) |> get(private_permalink_path(build_conn(), :query, token)) |> response(200) =~ query.statement
    end

    test "it renders not found for invalid public tokens" do
      assert build_conn() |> get(public_permalink_path(build_conn(), :query, "invalid")) |> response(404)
    end

    test "it renders not found for invalid private tokens" do
      assert login(create_user!()) |> get(private_permalink_path(build_conn(), :query, "invalid")) |> response(404)
    end

    test "the result does not include permalinks to avoid privilege escalation" do
      query = create_query!(create_user!())
      token = Air.Service.Token.public_query_token(query)

      result = build_conn() |> get(public_permalink_path(build_conn(), :query, token)) |> response(200)
      refute result =~ "/permalink"
    end
  end
end

defmodule Air.Service.Export.Test do
  use Air.SchemaCase, async: true

  alias Air.Service.{AuditLog, Export}
  import Air.TestRepoHelper

  setup do
    {:ok, %{user: create_user!()}}
  end

  test "includes user information", %{user: user} do
    assert export(user)["user"]["name"] == user.name
  end

  test "includes user group information" do
    group1 = create_group!()
    group2 = create_group!()
    data_source = create_data_source!(%{groups: [group1.id]})
    user = create_user!(%{groups: [group1.id, group2.id]})

    assert export(user)["user"]["groups"] == [
             %{"name" => group1.name, "data_sources" => [%{"name" => data_source.name, "id" => data_source.id}]},
             %{"name" => group2.name, "data_sources" => []}
           ]
  end

  test "includes audit logs", %{user: user} do
    AuditLog.log(user, "Some audit log", %{some: "metadata"})
    AuditLog.log(user, "Some other audit log", %{some: "metadata"})

    assert [
             %{"event" => "Some audit log", "metadata" => %{"some" => "metadata"}},
             %{"event" => "Some other audit log"}
           ] = export(user)["audit_logs"]
  end

  test "includes queries", %{user: user} do
    create_query!(user, %{statement: "Some query"})
    create_query!(user, %{statement: "Some other query"})

    assert export(user)["queries"] |> Enum.map(& &1["statement"]) |> Enum.sort() == ["Some other query", "Some query"]
  end

  test "includes query result chunks", %{user: user} do
    query = create_query!(user)

    Repo.insert_all(Air.Schemas.ResultChunk, [
      %{query_id: query.id, index: 1, encoded_data: :zlib.gzip("some encoded data")}
    ])

    assert [%{"encoded_data" => "some encoded data"}] = export(user)["result_chunks"]
  end

  test "includes views", %{user: user} do
    data_source = create_data_source!()
    view = create_view!(user, data_source)

    view_name = view.name
    assert [%{"name" => ^view_name}] = export(user)["views"]
  end

  test "includes api tokens", %{user: user} do
    Air.Token.create_api_token(user, :api, "Some token")

    assert [%{"access" => "api", "description" => "Some token"}] = export(user)["api_tokens"]
  end

  defp export(user) do
    {:ok, result} =
      Export.reduce_while(user, [], fn next, acc ->
        {:cont, [acc, next]}
      end)

    result
    |> to_string()
    |> Poison.decode!()
  end
end

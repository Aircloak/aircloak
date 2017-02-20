defmodule Air.Service.QueryTest do
  use Air.SchemaCase, async: true

  import Air.TestRepoHelper
  alias Air.Service.Query

  describe "get" do
    test "loads existing queries" do
      user = create_user!()
      query = create_query!(user)
      query_id = query.id
      assert {:ok, %Air.Schemas.Query{id: ^query_id}} = Query.get(query_id)
    end

    test "of invalid id when the ID is garbage" do
      assert {:error, :invalid_id} == Query.get("missing")
    end

    test "of non-existent query returns not found" do
      assert {:error, :not_found} == Query.get(Ecto.UUID.generate())
    end
  end

  describe "currently_running" do
    test "returns running queries" do
      user = create_user!()
      query = create_query!(user)
      query_id = query.id
      assert [%Air.Schemas.Query{id: ^query_id}] = Query.currently_running()
    end

    test "does not return not running queries" do
      user = create_user!()
      create_query!(user, %{query_state: :completed})
      assert [] == Query.currently_running()
    end
  end
end

defmodule Air.Service.QueryTest do
  use Air.SchemaCase, async: true

  import Air.TestRepoHelper
  alias Air.Service.Query

  describe "get" do
    test "loads existing queries" do
      user = create_user!()
      query = create_query!(user)

      assert {:ok, loaded_query} = Query.get(query.id)
      assert loaded_query.id == query.id
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
      assert [running_query] = Query.currently_running()
      assert query.id == running_query.id
    end

    test "does not return not running queries" do
      user = create_user!()
      create_query!(user, %{completed: true})
      assert [] == Query.currently_running()
    end
  end

  describe "format_for_activity_monitor_view" do
    test "returns a list of maps for a list of queries" do
      data_source = create_data_source!()
      user = create_user!()
      query = create_query!(user, %{completed: true, data_source_id: data_source.id})
      assert [map] = Query.format_for_activity_monitor_view([query])
      assert map.id == query.id
      assert map.analyst_name == user.name
      assert map.data_source_name == data_source.name
    end
  end
end

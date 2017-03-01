defmodule Air.Service.QueryTest do
  use Air.SchemaCase, async: true

  import Air.TestRepoHelper
  alias Air.Service.Query

  describe "get_as_user" do
    setup do
      {:ok, %{user: create_user!()}}
    end

    test "loads existing queries", %{user: user} do
      query = create_query!(user)
      query_id = query.id
      assert {:ok, %Air.Schemas.Query{id: ^query_id}} = Query.get_as_user(user, query_id)
    end

    test "does not find other user's queries", %{user: user} do
      query = create_query!(_other_user = create_user!())
      assert {:error, :not_found} = Query.get_as_user(user, query.id)
    end

    test "finds all queries for admins", %{user: user} do
      query = create_query!(user)
      query_id = query.id
      assert {:ok, %Air.Schemas.Query{id: ^query_id}} = Query.get_as_user(create_admin_user!(), query_id)
    end

    test "of invalid id when the ID is garbage", %{user: user} do
      assert {:error, :invalid_id} == Query.get_as_user(user, "missing")
    end

    test "of non-existent query returns not found", %{user: user} do
      assert {:error, :not_found} == Query.get_as_user(user, Ecto.UUID.generate())
    end
  end

  describe "last_for_user" do
    test "returns last query the user issued" do
      user = create_user!()
      _previous_one = create_query!(user)
      last_one = create_query!(user)
      _later_one_by_another_user = create_query!(create_user!())

      assert last_one.id == Query.last_for_user(user).id
    end

    test "ignores other queries visible to an admin" do
      user = create_admin_user!()
      last_one = create_query!(user)
      _later_one_by_another_user = create_query!(create_user!())

      assert last_one.id == Query.last_for_user(user).id
    end

    test "nil if the user has no queries" do
      assert nil == Query.last_for_user(create_user!())
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
      create_query!(user, %{query_state: :error})
      create_query!(user, %{query_state: :cancelled})

      assert [] == Query.currently_running()
    end
  end

  describe "update_state" do
    test "changes the query_state" do
      query = create_query!(create_user!(), %{query_state: :started, data_source_id: create_data_source!().id})

      Query.update_state(query.id, :processing)

      assert {:ok, %{query_state: :processing}} = get_query(query.id)
    end

    test "it's impossible to change to an earlier state" do
      query = create_query!(create_user!(), %{query_state: :completed, data_source_id: create_data_source!().id})

      Query.update_state(query.id, :processing)

      assert {:ok, %{query_state: :completed}} = get_query(query.id)
    end
  end

  describe "process_result" do
    test "processing a successful result" do
      query = create_query!(create_user!(), %{query_state: :started, data_source_id: create_data_source!().id})

      Query.process_result(%{
        "query_id" => query.id,
        "columns" => ["col1", "col2"],
        "rows" => [%{"occurrences" => 10, "row" => [1, 1]}],
        "info" => ["some info"],
        "users_count" => 2,
        "features" => %{"selected_types" => ["some types"]},
        "execution_time" => 123,
      })

      {:ok, query} = get_query(query.id)
      assert %{
        query_state: :completed,
        execution_time: 123,
        users_count: 2,
        features: %{"selected_types" => ["some types"]},
      } = query
      assert %{
        "columns" => ["col1", "col2"],
        "rows" => [%{"occurrences" => 10, "row" => [1, 1]}],
        "info" => ["some info"],
        "row_count" => 10,
        "error" => nil,
        "types" => ["some types"],
      } = query.result
    end

    test "processing an error result" do
      query = create_query!(create_user!(), %{query_state: :started, data_source_id: create_data_source!().id})

      Query.process_result(%{
        "query_id" => query.id,
        "features" => %{"selected_types" => ["some types"]},
        "execution_time" => 123,
        "error" => "some reason",
      })

      {:ok, query} = get_query(query.id)
      assert %{
        query_state: :error,
        execution_time: 123,
        features: %{"selected_types" => ["some types"]},
        result: %{"error" => "some reason"},
      } = query
    end

    test "processing a cancelled result" do
      query = create_query!(create_user!(), %{query_state: :started, data_source_id: create_data_source!().id})

      Query.process_result(%{
        "query_id" => query.id,
        "features" => %{"selected_types" => ["some types"]},
        "execution_time" => 123,
        "cancelled" => true,
      })

      {:ok, query} = get_query(query.id)
      assert %{
        query_state: :cancelled,
        execution_time: 123,
        features: %{"selected_types" => ["some types"]},
        result: %{"error" => "Cancelled."},
      } = query
    end
  end
end

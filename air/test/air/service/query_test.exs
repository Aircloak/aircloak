defmodule Air.Service.QueryTest do
  use ExUnit.Case, async: false

  import Air.TestRepoHelper
  alias Air.Service.Query

  setup [:sandbox]

  describe "get_as_user" do
    setup [:sandbox, :with_user]

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
    setup [:sandbox]

    test "returns last query the user issued" do
      user = create_user!()
      _previous_one = create_query!(user)
      last_one = create_query!(user)
      _later_one_by_another_user = create_query!(create_user!())

      assert last_one.id == Query.last_for_user(user, :http).id
    end

    test "ignores other queries visible to an admin" do
      user = create_admin_user!()
      last_one = create_query!(user)
      _later_one_by_another_user = create_query!(create_user!())

      assert last_one.id == Query.last_for_user(user, :http).id
    end

    test "nil if the user has no queries" do
      assert nil == Query.last_for_user(create_user!(), :http)
    end

    test "does not return query in other contexts" do
      user = create_user!()
      _previous_one = create_query!(user)
      create_query!(user, %{context: :psql})

      assert nil == Query.last_for_user(create_user!(), :http)
    end
  end

  describe "currently_running/0" do
    setup [:sandbox]

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

  describe "currently_running/2" do
    setup [:sandbox, :with_user, :with_data_source]

    test "returns running queries on the given data source", context do
      query = create_query!(context.user, %{data_source_id: context.data_source.id})

      query_id = query.id

      assert [%Air.Schemas.Query{id: ^query_id}] = Query.currently_running(context.user, context.data_source, :http)
    end

    test "does not return not running queries", context do
      create_query!(context.user, %{
        data_source_id: context.data_source.id,
        query_state: :completed
      })

      create_query!(context.user, %{data_source_id: context.data_source.id, query_state: :error})

      create_query!(context.user, %{
        data_source_id: context.data_source.id,
        query_state: :cancelled
      })

      assert [] == Query.currently_running(context.user, context.data_source, :http)
    end

    test "does not return other users' queries", context do
      create_query!(_other_user = create_user!(), %{data_source_id: context.data_source.id})

      assert [] = Query.currently_running(context.user, context.data_source, :http)
    end

    test "does not return queries to other data sources", context do
      create_query!(context.user, %{data_source_id: _other_source = create_data_source!().id})

      assert [] = Query.currently_running(context.user, context.data_source, :http)
    end

    test "does not return queries in other contexts", context do
      create_query!(context.user, %{data_source_id: context.data_source.id, context: :psql})

      assert [] = Query.currently_running(context.user, context.data_source, :http)
    end
  end

  describe "update_state" do
    setup [:sandbox]

    test "changes the query_state" do
      query =
        create_query!(create_user!(), %{
          query_state: :started,
          data_source_id: create_data_source!().id
        })

      Query.update_state(query.id, :processing)

      assert {:ok, %{query_state: :processing}} = get_query(query.id)
    end

    test "it's impossible to change to an earlier state" do
      query =
        create_query!(create_user!(), %{
          query_state: :completed,
          data_source_id: create_data_source!().id
        })

      Query.update_state(query.id, :processing)

      assert {:ok, %{query_state: :completed}} = get_query(query.id)
    end

    Enum.each([:cancelled, :error, :completed], fn terminal_state ->
      Enum.each(
        [
          :started,
          :parsing,
          :compiling,
          :awaiting_data,
          :ingesting_data,
          :processing,
          :post_processing,
          :cancelled,
          :error,
          :completed
        ],
        fn state ->
          test "changing from terminal state '#{terminal_state}' to '#{state}' is not allowed" do
            params = %{
              query_state: unquote(terminal_state),
              data_source_id: create_data_source!().id
            }

            query = create_query!(create_user!(), params)
            Query.update_state(query.id, unquote(state))
            assert {:ok, %{query_state: unquote(terminal_state)}} = get_query(query.id)
          end
        end
      )
    end)
  end

  describe "process_result" do
    setup [:sandbox]

    test "processing a successful result" do
      query =
        create_query!(create_user!(), %{
          query_state: :started,
          data_source_id: create_data_source!().id
        })

      send_query_result(
        query.id,
        %{
          columns: ["col1", "col2"],
          info: ["some info"],
          features: %{selected_types: ["some types"]},
          execution_time: 123
        },
        [%{occurrences: 10, row: [1, 1]}]
      )

      {:ok, query} = get_query(query.id)

      assert %{
               query_state: :completed,
               execution_time: 123,
               features: %{"selected_types" => ["some types"]}
             } = query

      assert query.result == %{
               "columns" => ["col1", "col2"],
               "info" => ["some info"],
               "row_count" => 10,
               "error" => nil,
               "types" => ["some types"],
               "log" => ""
             }

      assert Query.buckets(query, :all) == [%{"occurrences" => 10, "row" => [1, 1]}]
    end

    test "processing an error result" do
      query =
        create_query!(create_user!(), %{
          query_state: :started,
          data_source_id: create_data_source!().id
        })

      log =
        ExUnit.CaptureLog.capture_log(fn ->
          send_query_result(query.id, %{
            features: %{"selected_types" => ["some types"]},
            execution_time: 123,
            error: "some reason"
          })
        end)

      assert log =~ ~S("message":"some reason")

      {:ok, query} = get_query(query.id)

      assert %{
               query_state: :error,
               execution_time: 123,
               features: %{"selected_types" => ["some types"]},
               result: %{"error" => "some reason"}
             } = query
    end

    test "processing a cancelled result" do
      query =
        create_query!(create_user!(), %{
          query_state: :started,
          data_source_id: create_data_source!().id
        })

      send_query_result(query.id, %{
        features: %{"selected_types" => ["some types"]},
        execution_time: 123,
        cancelled: true
      })

      {:ok, query} = get_query(query.id)

      assert %{
               query_state: :cancelled,
               execution_time: 123,
               features: %{"selected_types" => ["some types"]},
               result: %{"error" => "Cancelled."}
             } = query
    end

    test "results of completed queries are ignored" do
      query =
        create_query!(create_user!(), %{
          query_state: :error,
          data_source_id: create_data_source!().id
        })

      send_query_result(query.id, %{
        features: %{"selected_types" => ["some types"]},
        execution_time: 123,
        cancelled: true
      })

      assert {:ok, %{query_state: :error}} = get_query(query.id)
    end
  end

  describe "query_died" do
    setup [:sandbox]

    test "ignores completed queries" do
      query =
        create_query!(create_user!(), %{
          query_state: :completed,
          data_source_id: create_data_source!().id
        })

      Query.query_died(query.id)

      {:ok, query} = get_query(query.id)
      assert %{query_state: :completed} = query
    end

    test "sets the result" do
      query =
        create_query!(create_user!(), %{
          query_state: :started,
          data_source_id: create_data_source!().id
        })

      Query.query_died(query.id)

      {:ok, query} = get_query(query.id)

      assert %{
               query_state: :error,
               result: %{"error" => "Query died."}
             } = query
    end
  end

  def sandbox(_context) do
    Ecto.Adapters.SQL.Sandbox.checkout(Air.Repo)
    Ecto.Adapters.SQL.Sandbox.mode(Air.Repo, {:shared, self()})
    :ok
  end

  def with_user(_context) do
    {:ok, user: create_user!()}
  end

  def with_data_source(_context) do
    {:ok, data_source: create_data_source!()}
  end
end

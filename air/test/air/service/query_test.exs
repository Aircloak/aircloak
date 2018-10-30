defmodule Air.Service.QueryTest do
  use ExUnit.Case, async: false

  import Air.TestRepoHelper
  alias Air.Service.Query

  setup [:sandbox]

  describe "create" do
    setup [:with_user]

    test "cannot create query for disabled user", %{user: user, data_source: data_source} do
      assert {:ok, _} = Air.Service.User.disable(user)

      assert {:error, :unable_to_create_query} =
               Air.Service.Query.create({:id, data_source.id}, :autogenerate, user, nil, nil, nil, [])

      Air.Service.DataSource.QueryScheduler.sync()
    end

    test "cannot create query for an unpermitted data source", %{user: user} do
      data_source = create_data_source!()

      assert {:error, :unauthorized} =
               Air.Service.Query.create({:id, data_source.id}, :autogenerate, user, nil, nil, nil, [])

      Air.Service.DataSource.QueryScheduler.sync()
    end

    test "time spent defaults to 0 for all states", %{user: user, data_source: data_source} do
      assert {:ok,
              %{
                time_spent: %{
                  "created" => 0,
                  "started" => 0,
                  "parsing" => 0,
                  "compiling" => 0,
                  "awaiting_data" => 0,
                  "ingesting_data" => 0,
                  "processing" => 0,
                  "post_processing" => 0,
                  "completed" => 0,
                  "error" => 0,
                  "cancelled" => 0
                }
              }} = Air.Service.Query.create({:id, data_source.id}, :autogenerate, user, :http, "", %{}, [])

      Air.Service.DataSource.QueryScheduler.sync()
    end
  end

  describe "get_as_user" do
    setup [:with_user]

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

  describe "get" do
    test "loads existing queries" do
      query = create_query!(create_user!())
      query_id = query.id
      assert {:ok, %Air.Schemas.Query{id: ^query_id}} = Query.get(query_id)
    end

    test "when the ID is garbage" do
      assert {:error, :invalid_id} == Query.get("missing")
    end

    test "of non-existent query" do
      assert {:error, :not_found} == Query.get(Ecto.UUID.generate())
    end
  end

  describe "last_for_user" do
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
    test "returns running queries" do
      user = create_user!()
      _not_started_query = create_query!(user)
      started_query = create_query!(user, %{query_state: :started})
      assert [%Air.Schemas.Query{id: query_id}] = Query.currently_running()
      assert query_id == started_query.id
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
    setup [:with_user, :with_data_source]

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
    test "changes the query_state" do
      query = create_query!(create_user!(), %{query_state: :started})

      Query.update_state(query.id, :processing)

      assert {:ok, %{query_state: :processing}} = get_query(query.id)
    end

    test "it's impossible to change to an earlier state" do
      query = create_query!(create_user!(), %{query_state: :completed})

      Query.update_state(query.id, :processing)

      assert {:ok, %{query_state: :completed}} = get_query(query.id)
    end

    for terminal_state <- ~w(cancelled error completed)a,
        state <- ~w(started parsing compiling awaiting_data ingesting_data processing post_processing cancelled
          error completed) do
      test "changing from terminal state '#{terminal_state}' to '#{state}' is not allowed" do
        query = create_query!(create_user!(), %{query_state: unquote(terminal_state)})

        Query.update_state(query.id, unquote(state))

        assert {:ok, %{query_state: unquote(terminal_state)}} = get_query(query.id)
      end
    end

    test "records time spent in previous state" do
      query = create_query!(create_user!(), %{query_state: :awaiting_data})

      :timer.sleep(100)
      Query.update_state(query.id, :processing)

      assert {:ok, %{time_spent: %{"awaiting_data" => time}}} = get_query(query.id)
      assert time >= 100
    end
  end

  describe "process_result" do
    test "processing a successful result" do
      query = create_query!(create_user!(), %{query_state: :started})

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

    test "records time spent in previous state" do
      query = create_query!(create_user!(), %{query_state: :started})

      :timer.sleep(100)
      send_query_result(query.id, %{columns: [], info: [], features: %{}, execution_time: 123}, _rows = [])

      assert {:ok, %{time_spent: %{"started" => time}}} = get_query(query.id)
      assert time >= 100
    end

    test "processing an error result" do
      query = create_query!(create_user!(), %{query_state: :started})

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
      query = create_query!(create_user!(), %{query_state: :started})

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
      query = create_query!(create_user!(), %{query_state: :error})

      send_query_result(query.id, %{
        features: %{"selected_types" => ["some types"]},
        execution_time: 123,
        cancelled: true
      })

      assert {:ok, %{query_state: :error}} = get_query(query.id)
    end
  end

  describe "query_died" do
    test "ignores completed queries" do
      query = create_query!(create_user!(), %{query_state: :completed})

      Query.query_died(query.id)

      {:ok, query} = get_query(query.id)
      assert %{query_state: :completed} = query
    end

    test "sets the result" do
      query = create_query!(create_user!(), %{query_state: :started})

      Query.query_died(query.id)

      {:ok, query} = get_query(query.id)

      assert %{
               query_state: :error,
               result: %{"error" => "Query died."}
             } = query
    end
  end

  describe ".queries" do
    test "results are ordered from newest to oldest" do
      query1 = create_query!(create_user!())
      query2 = create_query!(create_user!())
      query3 = create_query!(create_user!())

      assert Query.queries(filters()) |> Enum.map(& &1.id) == [query3.id, query2.id, query1.id]
    end

    test "filtering by query_state" do
      _query1 = create_query!(create_user!(), %{query_state: :started})
      query2 = create_query!(create_user!(), %{query_state: :error})
      query3 = create_query!(create_user!(), %{query_state: :completed})

      assert Query.queries(filters(%{query_states: [:error, :completed]})) |> Enum.map(& &1.id) == [
               query3.id,
               query2.id
             ]
    end

    test "filtering by data source" do
      _query1 = create_query!(create_user!())
      query2 = create_query!(create_user!())
      query3 = create_query!(create_user!())

      assert Query.queries(filters(%{data_sources: [query2.data_source_id, query3.data_source_id]}))
             |> Enum.map(& &1.id) == [query3.id, query2.id]
    end

    test "filtering by user" do
      _query1 = create_query!(create_user!())
      query2 = create_query!(create_user!())
      query3 = create_query!(create_user!())

      assert Query.queries(filters(%{users: [query2.user_id, query3.user_id]}))
             |> Enum.map(& &1.id) == [query3.id, query2.id]
    end

    test "max results" do
      _query1 = create_query!(create_user!())
      query2 = create_query!(create_user!())
      query3 = create_query!(create_user!())

      assert Query.queries(filters(%{max_results: 2})) |> Enum.map(& &1.id) == [query3.id, query2.id]
    end
  end

  describe ".users_for_filters" do
    test "includes users of matching queries" do
      _query1 = create_query!(create_user!())
      query2 = create_query!(create_user!(), %{query_state: :error})
      query3 = create_query!(create_user!(), %{query_state: :completed})

      assert Query.users_for_filters(filters(%{query_states: [:error, :completed], max_results: 1}))
             |> Enum.map(& &1.id)
             |> Enum.sort() == Enum.sort([query2.user_id, query3.user_id])
    end

    test "includes filtered users" do
      _user1 = create_user!()
      user2 = create_user!()
      user3 = create_user!()

      assert Query.users_for_filters(filters(%{users: [user2.id, user3.id]}))
             |> Enum.map(& &1.id)
             |> Enum.sort() == Enum.sort([user2.id, user3.id])
    end

    test "results are orderer by name" do
      users = Enum.map(1..3, fn _ -> create_user!() end)

      assert Query.users_for_filters(filters(%{users: Enum.map(users, & &1.id)})) |> Enum.map(& &1.name) ==
               users |> Enum.map(& &1.name) |> Enum.sort()
    end
  end

  describe ".data_sources_for_filters" do
    test "includes data_sources of matching queries" do
      _query1 = create_query!(create_data_source!())
      query2 = create_query!(create_data_source!(), %{query_state: :error})
      query3 = create_query!(create_data_source!(), %{query_state: :completed})

      assert Query.data_sources_for_filters(filters(%{query_states: [:error, :completed], max_results: 1}))
             |> Enum.map(& &1.id)
             |> Enum.sort() == Enum.sort([query2.data_source_id, query3.data_source_id])
    end

    test "includes filtered data_sources" do
      _data_source1 = create_data_source!()
      data_source2 = create_data_source!()
      data_source3 = create_data_source!()

      assert Query.data_sources_for_filters(filters(%{data_sources: [data_source2.id, data_source3.id]}))
             |> Enum.map(& &1.id)
             |> Enum.sort() == Enum.sort([data_source2.id, data_source3.id])
    end

    test "results are orderer by name" do
      data_sources = Enum.map(1..3, fn _ -> create_data_source!() end)

      assert Query.data_sources_for_filters(filters(%{data_sources: Enum.map(data_sources, & &1.id)}))
             |> Enum.map(& &1.name) == data_sources |> Enum.map(& &1.name) |> Enum.sort()
    end
  end

  describe ".for_display" do
    test "for_display of a finished query",
      do:
        Enum.each(
          [:error, :completed, :cancelled],
          &assert(%{completed: true} = display(%{query_state: &1}))
        )

    test "for_display of an unfinished query",
      do:
        Enum.each(
          [
            :created,
            :started,
            :parsing,
            :compiling,
            :awaiting_data,
            :ingesting_data,
            :processing,
            :post_processing
          ],
          &assert(%{completed: false} = display(%{query_state: &1}))
        )

    test "for_display includes all data from result",
      do: assert(%{"some" => "data"} = display(%{result: %{"some" => "data"}}))

    test "for_display when result is not preloaded" do
      query = create_user!() |> create_query!(%{result: %{rows: [], columns: []}})
      query_without_result = Air.Repo.get!(Air.Schemas.Query, query.id)
      display = Query.for_display(query_without_result)

      assert :error == Map.fetch(display, :rows)
      assert :error == Map.fetch(display, :columns)
    end

    test "includes the owner's name" do
      user = create_user!()
      assert Query.for_display(create_query!(user)).user.name == user.name
    end

    test "includes the data source name" do
      data_source = create_data_source!()
      assert display(%{data_source_id: data_source.id}).data_source.name == data_source.name
    end

    defp display(create_query_params),
      do:
        create_user!()
        |> create_query!(create_query_params)
        |> Query.for_display()
  end

  describe ".awaiting_start" do
    test "fetching not started queries" do
      user = create_user!()
      q1 = create_query!(user)
      q2 = create_query!(user)
      create_query!(user, %{query_state: :completed})
      assert Query.awaiting_start() |> Enum.map(& &1.id) |> Enum.sort() == Enum.sort([q1.id, q2.id])
    end
  end

  defp filters(overrides \\ %{}) do
    Map.merge(
      %{
        query_states: [],
        data_sources: [],
        users: [],
        max_results: 100,
        from: Timex.now() |> Timex.shift(days: -1),
        to: Timex.now() |> Timex.shift(days: 1)
      },
      overrides
    )
  end

  def sandbox(_context) do
    Ecto.Adapters.SQL.Sandbox.checkout(Air.Repo)
    Ecto.Adapters.SQL.Sandbox.mode(Air.Repo, {:shared, self()})
    :ok
  end

  def with_user(_context) do
    group = create_group!()
    user = create_user!(%{groups: [group.id]})
    data_source = create_data_source!(%{groups: [group.id]})
    {:ok, user: user, data_source: data_source}
  end

  def with_data_source(_context) do
    {:ok, data_source: create_data_source!()}
  end
end

defmodule IntegrationTest.ViewTest do
  use ExUnit.Case, async: true

  alias IntegrationTest.Manager

  setup_all do
    {:ok, user: Manager.create_air_user()}
  end

  test "name error", context do
    assert {:error, %Ecto.Changeset{errors: [name: {error, _}]}} = create_view(context.user, "users", "sql")
    assert error == "has already been taken"
  end

  test "sql syntax error", context do
    assert {:error, %Ecto.Changeset{errors: [sql: {error, _}]}} = create_view(context.user, "foo", "select")
    assert error =~ ~r/Expected `column definition`/
  end

  test "successful saving of the new view", context do
    assert {:ok, view} = create_view(context.user, unique_view_name(), "select user_id, name from users")
    assert view.result_info.columns == [
      %{name: "user_id", type: "text", user_id: true},
      %{name: "name", type: "text", user_id: false}
    ]
  end

  test "cannot insert two views with the same name", context do
    {:ok, view} = create_view(context.user, unique_view_name(), "select user_id, name from users")
    assert {:error, changeset} = create_view(context.user, view.name, "select user_id, name from users")
    assert {"has already been taken", _} = Keyword.fetch!(changeset.errors, :name)
  end

  test "updating the view", context do
    {:ok, view} = create_view(context.user, unique_view_name(), "select user_id, name from users")

    new_name = unique_view_name()
    new_sql = "select user_id from users"
    assert {:ok, updated_view} = update_view(view.id, context.user, new_name, new_sql)

    assert updated_view.id == view.id
    assert updated_view.name == new_name
    assert updated_view.sql == new_sql
  end

  test "selecting from the view", context do
    {:ok, view} = create_view(context.user, unique_view_name(), "select user_id, name from users")
    {:ok, result} = run_query(context.user, "select name from #{view.name}")
    assert [%{"occurrences" => 100, "row" => ["john"]}] = result.buckets
  end

  defp unique_view_name(), do:
    "view_#{:erlang.unique_integer([:positive])}"

  defp create_view(user, name, sql), do:
    Air.Service.View.create(user, Manager.data_source(), name, sql)

  defp update_view(view_id, user, name, sql), do:
    Air.Service.View.update(view_id, user, name, sql)

  defp run_query(user, query, params \\ []) do
    {:ok, query} = Air.Service.Query.create(:autogenerate, user, :http, query, params, [])
    Air.Service.DataSource.run_query(query, {:name, Manager.data_source_name()})
  end
end

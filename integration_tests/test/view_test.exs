defmodule IntegrationTest.ViewTest do
  use ExUnit.Case, async: true

  alias IntegrationTest.Manager
  import IntegrationTest.Helpers

  setup_all do
    {:ok, user: Manager.create_admin_user()}
  end

  test "name error", context do
    assert {:error, %Ecto.Changeset{errors: [name: {error, _}]}} = create_view(context.user, "users", "sql")

    assert error == "has already been taken"
  end

  test "sql syntax error", context do
    assert {:error, %Ecto.Changeset{errors: [sql: {error, _}]}} = create_view(context.user, "foo", "select")

    assert error =~ ~r/Expected a column definition/
  end

  test "successful saving of the new view", context do
    assert {:ok, view} =
             create_view(context.user, unique_name(:view), "select user_id, name from users", "view comment")

    assert [%{name: "user_id", type: "text", key_type: "user_id"}, %{name: "name", type: "text", key_type: nil}] =
             view.columns

    assert view.comment == "view comment"
  end

  test "cannot insert two views with the same name", context do
    {:ok, view} = create_view(context.user, unique_name(:view), "select user_id, name from users")

    assert {:error, changeset} = create_view(context.user, view.name, "select user_id, name from users")

    assert {"has already been taken", _} = Keyword.fetch!(changeset.errors, :name)
  end

  test "updating the view", context do
    {:ok, view} = create_view(context.user, unique_name(:view), "select user_id, name from users", "old comment")

    new_name = unique_name(:view)
    new_sql = "select user_id from users"
    new_comment = "new comment"
    assert {:ok, updated_view} = update_view(view.id, context.user, new_name, new_sql, new_comment)

    assert updated_view.id == view.id
    assert updated_view.name == new_name
    assert updated_view.sql == new_sql
    assert updated_view.comment == new_comment
  end

  test "selecting from the view", context do
    {:ok, view} = create_view(context.user, unique_name(:view), "select user_id, name from users")
    {:ok, result} = run_query(context.user, "select name from #{view.name}")
    assert [%{"occurrences" => 100, "row" => ["john"]}] = result.buckets
  end

  test "view name can't be the same as a name of an existing table", context do
    name = unique_name(:view)

    {:ok, _} =
      Air.Service.AnalystTable.create(context.user, Manager.data_source(), name, "select * from users", "comment")

    assert {:error, changeset} = create_view(context.user, name, "select * from users")
    assert {"has already been taken", _} = changeset.errors[:name]
  end

  defp create_view(user, name, sql, comment \\ nil),
    do: Air.Service.View.create(user, Manager.data_source(), name, sql, comment)

  defp update_view(view_id, user, name, sql, comment),
    do: Air.Service.View.update(view_id, user, name, sql, comment)
end

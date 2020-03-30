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

    assert error =~ ~r/Expected `column definition`/
  end

  test "successful saving of the new view", context do
    assert {:ok, view} = create_view(context.user, unique_name(:view), "select user_id, name from users")

    assert [%{name: "user_id", type: "text", key_type: "user_id"}, %{name: "name", type: "text", key_type: nil}] =
             view.columns
  end

  test "cannot insert two views with the same name", context do
    {:ok, view} = create_view(context.user, unique_name(:view), "select user_id, name from users")

    assert {:error, changeset} = create_view(context.user, view.name, "select user_id, name from users")

    assert {"has already been taken", _} = Keyword.fetch!(changeset.errors, :name)
  end

  test "updating the view", context do
    {:ok, view} = create_view(context.user, unique_name(:view), "select user_id, name from users")

    new_name = unique_name(:view)
    new_sql = "select user_id from users"
    assert {:ok, updated_view} = update_view(view.id, context.user, new_name, new_sql)

    assert updated_view.id == view.id
    assert updated_view.name == new_name
    assert updated_view.sql == new_sql
  end

  test "selecting from the view", context do
    {:ok, view} = create_view(context.user, unique_name(:view), "select user_id, name from users")
    {:ok, result} = run_query(context.user, "select name from #{view.name}")
    assert [%{"occurrences" => 100, "row" => ["john"]}] = result.buckets
  end

  test "select * ignores excluded columns", context do
    name = unique_name(:view)
    {:ok, view} = create_view(context.user, name, "select * from column_access")

    columns = Enum.map(view.columns, & &1.name)
    assert "black" not in columns
  end

  test "cannot select excluded columns", context do
    name = unique_name(:view)
    {:error, changeset} = create_view(context.user, name, "select white, black from column_access")
    assert changeset.errors[:sql] == {"Column `black` doesn't exist in table `column_access`.", []}
  end

  test "view name can't be the same as a name of an existing table", context do
    name = unique_name(:view)
    {:ok, _} = Air.Service.AnalystTable.create(context.user, Manager.data_source(), name, "select * from users")
    assert {:error, changeset} = create_view(context.user, name, "select * from users")
    assert {"has already been taken", _} = changeset.errors[:name]
  end

  defp create_view(user, name, sql), do: Air.Service.View.create(user, Manager.data_source(), name, sql)

  defp update_view(view_id, user, name, sql), do: Air.Service.View.update(view_id, user, name, sql)
end

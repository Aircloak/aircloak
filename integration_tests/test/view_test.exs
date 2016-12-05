defmodule IntegrationTest.ViewTest do
  use ExUnit.Case, async: true

  alias IntegrationTest.Manager
  alias Air.Schemas.View

  setup_all do
    {:ok, user: Manager.create_air_user()}
  end

  test "reporting view error", context do
    assert {:error, %Ecto.Changeset{errors: [sql: {error, _}]}} = create_view(context.user, "foo", "select")
    assert error =~ ~r/Expected `column definition`/
  end

  test "successful saving of the new view", context, do:
    assert {:ok, %View{}} = create_view(context.user, unique_view_name(), "select user_id, name from users")

  test "updating the view", context do
    {:ok, view} = create_view(context.user, unique_view_name(), "select user_id, name from users")

    new_name = unique_view_name()
    new_sql = "select user_id from users"
    assert {:ok, updated_view} = update_view(context.user, view.id, new_name, new_sql)

    assert updated_view.id == view.id
    assert updated_view.name == new_name
    assert updated_view.sql == new_sql
  end

  test "selecting from the view", context do
    {:ok, view} = create_view(context.user, unique_view_name(), "select user_id, name from users")
    {:ok, result} = run_query(context.user, "select name from #{view.name}")
    assert Map.fetch!(result, "rows") == [%{"occurrences" => 100, "row" => ["john"]}]
  end

  defp unique_view_name(), do:
    "view_#{:erlang.unique_integer([:positive])}"

  defp create_view(user, name, sql), do:
    Air.Service.DataSource.create_view(Manager.data_source_local_id(), user, name, sql)

  defp update_view(user, view_id, name, sql), do:
    Air.Service.DataSource.update_view(user, view_id, name, sql)

  defp run_query(user, query, params \\ []), do:
    Air.Service.DataSource.run_query(
      {:global_id, Manager.data_source_global_id()},
      user,
      query,
      params
    )
end

defmodule IntegrationTest.ViewTest do
  use ExUnit.Case, async: true

  alias IntegrationTest.Manager
  alias Air.Schemas.View

  test "reporting view error" do
    assert {:error, error} = create_view("foo", "select")
    assert error =~ ~r/Expected `column definition`/
  end

  test "successful saving of the new view", do:
    assert {:ok, %View{}} = create_view(unique_view_name(), "select user_id, name from users")

  test "updating the view" do
    {:ok, view} = create_view(unique_view_name(), "select user_id, name from users")

    new_name = unique_view_name()
    assert {:ok, updated_view} = update_view(view.id, new_name, "select user_id from users")

    assert updated_view.id == view.id
    assert updated_view.name == new_name
    assert updated_view.sql == "select user_id from users"
  end


  defp unique_view_name(), do:
    "view_#{:erlang.unique_integer()}"

  defp create_view(name, sql), do:
    Air.Service.DataSource.create_view({:global_id, Manager.data_source_global_id()},  Manager.air_user(),
      name, sql)

  defp update_view(view_id, name, sql), do:
    Air.Service.DataSource.update_view({:global_id, Manager.data_source_global_id()},  Manager.air_user(),
      view_id, name, sql)
end

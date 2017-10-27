defmodule Air.ViewHelpersTest do
  use AirWeb.ConnCase, async: true

  alias Air.TestRepoHelper
  alias AirWeb.ViewHelpers

  @table_name "test_table"

  describe "selectables for tables" do
    setup [:normal_setup]

    test "empty list for data source without tables", context, do:
      assert [] == ViewHelpers.selectables(context.conn, TestRepoHelper.create_data_source!())

    test "lists selectables that belong to a data source", context, do:
      assert [%{id: @table_name, view: false}] =
        ViewHelpers.selectables(context.conn, context.data_source)

    test "no views amongst selectables if none is setup", context, do:
      assert [] = only_views(context)
  end

  describe "selectables for views" do
    setup [:normal_setup, :create_view]

    test "includes views in selectables", context do
      view_name = context.view.name
      assert [%{id: ^view_name}] = only_views(context)
    end

    test "filters out a selectable by internal_id", context, do:
      assert [] = only_views(context, context.view.id)

    test "views contain edit link", context do
      [view_selectable] = only_views(context)
      assert view_selectable.edit_link
    end

    test "views contain delete html", context do
      [view_selectable] = only_views(context)
      assert view_selectable.delete_html
    end
  end

  defp only_views(context, filter_name \\ nil), do:
    ViewHelpers.selectables(context.conn, context.data_source, filter_name)
    |> Enum.filter(& &1.view)

  defp normal_setup(context) do
    group = TestRepoHelper.create_group!()
    user = TestRepoHelper.create_user!(%{groups: [group.id]})
    conn = Map.put(build_conn(), :assigns, %{current_user: user})

    tables = Poison.encode!([%{
      id: @table_name,
      columns: [%{
        name: "uid",
        type: "integer",
        user_id: true
      }],
    }])
    data_source = TestRepoHelper.create_data_source!(%{tables: tables, groups: [group.id]})

    {:ok, Map.merge(context, %{
      conn: conn,
      user: user,
      data_source: data_source,
    })}
  end

  defp create_view(context) do
    view = TestRepoHelper.create_view!(context.user, context.data_source)
    {:ok, Map.put(context, :view, view)}
  end
end

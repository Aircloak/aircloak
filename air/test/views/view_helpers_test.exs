defmodule Air.ViewHelpersTest do
  use AirWeb.ConnCase, async: true

  alias Air.TestRepoHelper
  alias AirWeb.ViewHelpers

  @table_name "test_table"

  describe "selectables for tables" do
    setup [:normal_setup]

    test(
      "empty list for data source without tables",
      context,
      do: assert([] == ViewHelpers.selectables(context.conn, TestRepoHelper.create_data_source!()))
    )

    test(
      "lists selectables that belong to a data source",
      context,
      do:
        assert(
          [%{id: @table_name, analyst_created: false}] = ViewHelpers.selectables(context.conn, context.data_source)
        )
    )

    test(
      "no analyst created selectables amongst selectables if none have been setup",
      context,
      do: assert([] = only_analyst_created(context))
    )
  end

  describe "selectables for views" do
    setup [:normal_setup, :create_view]

    test "includes views in selectables", context do
      view_name = context.view.name
      assert [%{id: ^view_name}] = only_analyst_created(context)
    end
  end

  describe "selectables for analyst table" do
    setup [:normal_setup, :create_analyst_table]

    test "includes analyst table in selectables", context do
      table_name = context.analyst_table.name
      assert [%{id: ^table_name}] = only_analyst_created(context)
    end
  end

  describe "selectables for analyst created selectables" do
    setup [:normal_setup, :create_view]

    test(
      "filters out a selectable by internal_id",
      context,
      do: assert([] = only_analyst_created(context, context.view.id))
    )

    test "selectable contains edit link", context do
      [selectable] = only_analyst_created(context)
      assert selectable.edit_link
    end

    test "selectable contains delete html", context do
      [selectable] = only_analyst_created(context)
      assert selectable.delete_html
    end
  end

  defp only_analyst_created(context, filter_name \\ nil),
    do:
      ViewHelpers.selectables(context.conn, context.data_source, filter_name)
      |> Enum.filter(& &1.analyst_created)

  defp normal_setup(context) do
    group = TestRepoHelper.create_group!()
    user = TestRepoHelper.create_user!(%{groups: [group.id]})
    conn = Map.put(build_conn(), :assigns, %{current_user: user})

    tables =
      Jason.encode!([
        %{
          id: @table_name,
          columns: [
            %{
              name: "uid",
              type: "integer",
              user_id: true
            }
          ]
        }
      ])

    data_source = TestRepoHelper.create_data_source!(%{tables: tables, groups: [group.id]})

    {:ok,
     Map.merge(context, %{
       conn: conn,
       user: user,
       data_source: data_source
     })}
  end

  defp create_view(context) do
    view = TestRepoHelper.create_view!(context.user, context.data_source)
    {:ok, Map.put(context, :view, view)}
  end

  defp create_analyst_table(context) do
    analyst_table = TestRepoHelper.create_analyst_table!(context.user, context.data_source)
    {:ok, Map.put(context, :analyst_table, analyst_table)}
  end
end

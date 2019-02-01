defmodule AirWeb.SelectableController do
  @moduledoc false

  use Air.Web, :controller

  alias Air.Service.{View, AnalystTable}

  plug(:load_data_source)
  plug(:put_layout, "raw.html")

  # -------------------------------------------------------------------
  # AirWeb.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions, do: %{user: :all}

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def new(conn, %{"kind" => kind}),
    do:
      render(
        conn,
        "new.html",
        kind: kind,
        changeset: new_changeset_of_kind(kind),
        data_source: conn.assigns.data_source
      )

  def edit(conn, %{"id" => id, "kind" => kind}),
    do:
      render(
        conn,
        "edit.html",
        kind: kind,
        changeset: existing_changeset_of_kind(id, kind),
        data_source: conn.assigns.data_source
      )

  def create(conn, %{"kind" => kind} = params) do
    case create_selectable(conn, kind, get_name_and_sql(params, kind)) do
      {:ok, _selectable} ->
        redirect(conn, to: data_source_path(conn, :show, conn.assigns.data_source.name))

      {:error, changeset} ->
        render(conn, "new.html", kind: kind, changeset: changeset, data_source: conn.assigns.data_source)
    end
  end

  def update(conn, %{"id" => id, "kind" => kind} = params) do
    case update_selectable(conn, kind, id, get_name_and_sql(params, kind)) do
      {:ok, _selectable} ->
        conn
        |> maybe_broken_message()
        |> redirect(to: data_source_path(conn, :show, conn.assigns.data_source.name))

      {:error, changeset} ->
        render(
          conn,
          "edit.html",
          kind: kind,
          changeset: changeset,
          data_source: conn.assigns.data_source
        )
    end
  end

  def delete(conn, %{"id" => id, "kind" => "analyst_table"}),
    do:
      conn
      |> put_flash(:error, "Deleting analyst created tables is currently not supported.")
      |> redirect(to: data_source_path(conn, :show, conn.assigns.data_source.name))

  def delete(conn, %{"id" => id, "kind" => kind}) do
    View.delete(id, conn.assigns.current_user, revalidation_timeout: :timer.seconds(5))

    path =
      case get_req_header(conn, "referer") do
        [] -> data_source_path(conn, :show, conn.assigns.data_source.name)
        [url | _] -> URI.parse(url).path
      end

    conn |> maybe_broken_message() |> redirect(to: path)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp load_data_source(conn, _opts) do
    data_source_name = Map.fetch!(conn.params, "data_source_id")

    case Air.Service.DataSource.fetch_as_user(
           {:name, data_source_name},
           conn.assigns.current_user
         ) do
      {:ok, data_source} ->
        id_to_exclude =
          case Map.get(conn.params, "id") do
            nil -> nil
            id_as_string -> String.to_integer(id_as_string)
          end

        conn
        |> assign(:data_source, data_source)
        |> assign(:view_to_exclude_from_selectables, id_to_exclude)

      _ ->
        conn
        |> put_flash(:error, "Data source not found.")
        |> redirect(to: data_source_path(conn, :index))
    end
  end

  defp maybe_broken_message(conn) do
    case View.broken(conn.assigns.current_user, conn.assigns.data_source) do
      [] ->
        conn

      broken ->
        broken_names = broken |> Enum.map(& &1.name) |> Enum.join(", ")

        put_flash(
          conn,
          :warn,
          "After this change the following views are invalid: #{broken_names}"
        )
    end
  end

  defp new_changeset_of_kind("analyst_table"), do: AnalystTable.new_changeset()
  defp new_changeset_of_kind("view"), do: View.new_changeset()

  defp existing_changeset_of_kind(id, "analyst_table"), do: AnalystTable.changeset(id)
  defp existing_changeset_of_kind(id, "view"), do: View.changeset(id)

  defp get_name_and_sql(params, kind) do
    %{"name" => name, "sql" => sql} = params[kind]
    {name, sql}
  end

  defp create_selectable(conn, "analyst_table", {name, sql}),
    do: AnalystTable.create(conn.assigns.current_user, conn.assigns.data_source, name, sql)

  defp create_selectable(conn, "view", {name, sql}),
    do: View.create(conn.assigns.current_user, conn.assigns.data_source, name, sql)

  defp update_selectable(conn, "analyst_table", id, {name, sql}),
    do: AnalystTable.update(id, conn.assigns.current_user, name, sql)

  defp update_selectable(conn, "view", id, {name, sql}),
    do: View.update(id, conn.assigns.current_user, name, sql, revalidation_timeout: :timer.seconds(5))
end

defmodule Air.ViewController do
  @moduledoc false

  use Air.Web, :controller

  alias Air.Service.View

  plug :load_data_source
  plug :put_layout, "raw.html"


  # -------------------------------------------------------------------
  # Air.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions, do:
    %{user: :all}


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def new(conn, _params), do:
    render(conn, "new.html", changeset: View.new_changeset(), data_source: conn.assigns.data_source)

  def edit(conn, %{"id" => id}), do:
    render(conn, "edit.html", changeset: View.changeset(id), data_source: conn.assigns.data_source)

  def create(conn, %{"view" => %{"name" => name, "sql" => sql}}) do
    case View.create(conn.assigns.current_user, conn.assigns.data_source, name, sql) do
      {:ok, _view} ->
        redirect(conn, to: data_source_path(conn, :show, conn.assigns.data_source.id))
      {:error, changeset} ->
        render(conn, "new.html", changeset: changeset, data_source: conn.assigns.data_source)
    end
  end

  def update(conn, %{"id" => id, "view" => %{"name" => name, "sql" => sql}}) do
    case View.update(id, conn.assigns.current_user, name, sql) do
      {:ok, _view} ->
        conn
        |> maybe_broken_message()
        |> redirect(to: data_source_path(conn, :show, conn.assigns.data_source))
      {:error, changeset} ->
        render(conn, "edit.html", changeset: changeset, data_source: conn.assigns.data_source, view_id: id)
    end
  end

  def delete(conn, %{"id" => id}) do
    View.delete(id, conn.assigns.current_user)
    case get_req_header(conn, "referer") do
      [] -> redirect(conn, to: data_source_path(conn, :show, conn.assigns.data_source.id))
      [url|_] -> redirect(conn, external: url)
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp load_data_source(conn, _opts) do
    data_source_id = Map.fetch!(conn.params, "data_source_id")
    case Air.Service.DataSource.fetch_as_user({:id, data_source_id}, conn.assigns.current_user) do
      {:ok, data_source} ->
        selectable_views = case Map.fetch(conn.params, "id") do
          :error -> View.all(conn.assigns.current_user, data_source)
          {:ok, current_view_id_str} ->
            Enum.reject(
              View.all(conn.assigns.current_user, data_source),
              &(&1.id == String.to_integer(current_view_id_str))
            )
        end

        conn
        |> assign(:data_source, data_source)
        |> assign(:views, selectable_views)

      _ ->
        conn
        |> put_flash(:error, "Data source not found.")
        |> redirect(to: data_source_path(conn, :index))
    end
  end

  defp maybe_broken_message(conn) do
    case View.broken(conn.assigns.current_user, conn.assigns.data_source) do
      [] -> conn
      broken ->
        broken_names = broken |> Enum.map(&(&1.name)) |> Enum.join(", ")
        put_flash(conn, :warn, "After this change the following views are invalid: #{broken_names}")
    end
  end
end

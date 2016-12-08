defmodule Air.ViewController do
  @moduledoc false

  use Air.Web, :controller

  alias Air.Service.View

  plug :load_data_source


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
    case View.create(conn.assigns.current_user, conn.assigns.data_source.id, name, sql) do
      {:ok, _view} ->
        redirect(conn, to: data_source_path(conn, :show, conn.assigns.data_source.id))
      {:error, changeset} ->
        render(conn, "new.html", changeset: changeset, data_source: conn.assigns.data_source)
    end
  end

  def update(conn, %{"id" => id, "view" => %{"name" => name, "sql" => sql}}) do
    case View.update(id, conn.assigns.current_user, name, sql) do
      {:ok, _view} ->
        redirect(conn, to: data_source_path(conn, :show, conn.assigns.data_source))
      {:error, changeset} ->
        render(conn, "edit.html", changeset: changeset, data_source: conn.assigns.data_source, view_id: id)
    end
  end

  def delete(conn, %{"id" => id}) do
    View.delete(id, conn.assigns.current_user)
    redirect(conn, to: data_source_path(conn, :show, conn.assigns.data_source.id))
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp load_data_source(conn, _opts) do
    %{"data_source_id" => id} = conn.params
    {:ok, data_source} = Air.Service.DataSource.fetch_as_user({:id, id}, conn.assigns.current_user)

    conn
    |> assign(:data_source, data_source)
    |> assign(:views, View.all(conn.assigns.current_user, data_source))
  end
end

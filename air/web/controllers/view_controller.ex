defmodule Air.ViewController do
  @moduledoc false

  use Air.Web, :controller

  alias Air.Service.View


  # -------------------------------------------------------------------
  # Air.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions, do:
    %{user: :all}


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def new(conn, %{"data_source_id" => data_source_id}), do:
    render(conn, "new.html", changeset: View.new_changeset(), data_source_id: data_source_id)

  def edit(conn, %{"data_source_id" => data_source_id, "id" => id}), do:
    render(conn, "edit.html",
      changeset: View.changeset(id),
      data_source_id: data_source_id,
      view_id: id
    )

  def create(conn, params) do
    %{
      "data_source_id" => data_source_id,
      "view" => %{"name" => name, "sql" => sql}
    } = params

    case View.create(conn.assigns.current_user, data_source_id, name, sql) do
      {:ok, _view} ->
        redirect(conn, to: data_source_path(conn, :show, data_source_id))
      {:error, changeset} ->
        render(conn, "new.html", changeset: changeset, data_source_id: data_source_id)
    end
  end

  def update(conn, params) do
    %{
      "data_source_id" => data_source_id,
      "id" => id,
      "view" => %{"name" => name, "sql" => sql}
    } = params

    view = View.get(id)
    true = (view.user_id == conn.assigns.current_user.id)

    case View.update(view, conn.assigns.current_user, name, sql) do
      {:ok, _view} ->
        redirect(conn, to: data_source_path(conn, :show, data_source_id))
      {:error, changeset} ->
        render(conn, "edit.html", changeset: changeset, data_source_id: data_source_id, view_id: id)
    end
  end
end

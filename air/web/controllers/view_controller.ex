defmodule Air.ViewController do
  @moduledoc false

  use Air.Web, :controller

  alias Air.Schemas.View


  # -------------------------------------------------------------------
  # Air.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions, do:
    %{user: :all}


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def new(conn, %{"data_source_id" => data_source_id}), do:
    render(conn, "new.html", changeset: View.changeset(), data_source_id: data_source_id)

  def create(conn, params) do
    %{
      "data_source_id" => data_source_id,
      "view" => %{"name" => name, "sql" => sql}
    } = params

    case Air.Service.DataSource.create_view(data_source_id, conn.assigns.current_user, name, sql) do
      {:ok, _view} ->
        redirect(conn, to: data_source_path(conn, :show, data_source_id))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "new.html", changeset: changeset, data_source_id: data_source_id)

      {:error, reason} ->
        conn
        |> put_flash(:error, error_display(reason))
        |> render("new.html", changeset: View.changeset(), data_source_id: data_source_id)
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp error_display(:not_connected), do: "No cloak is available for the given data source."
end

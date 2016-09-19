defmodule Air.Admin.OrganisationController do
  @moduledoc false
  use Air.Web, :admin_controller

  alias Air.{Organisation, AuditLog}

  plug :scrub_params, "organisation" when action in [:create, :update]

  # -------------------------------------------------------------------
  # Air.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{
      org_admin: [:show],
      admin: :all
    }
  end

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    organisations = Repo.all(Organisation)
    render(conn, "index.html", organisations: organisations)
  end

  def new(conn, _params) do
    changeset = Organisation.changeset(%Organisation{})
    render(conn, "new.html", changeset: changeset)
  end

  def create(conn, %{"organisation" => organisation_params}) do
    changeset = Organisation.changeset(%Organisation{}, organisation_params)

    case Repo.insert(changeset) do
      {:ok, organisation} ->
        AuditLog.log(conn, "Created organisation", name: organisation.name)
        conn
        |> put_flash(:info, "Organisation created successfully.")
        |> redirect(to: admin_organisation_path(conn, :index))
      {:error, changeset} ->
        render(conn, "new.html", changeset: changeset)
    end
  end

  def show(conn, %{"id" => id}) do
    with %{halted: false} <- verify_org_permissions(conn, id) do
      organisation = Repo.get!(Organisation, id) |> Repo.preload([users: [:organisation, :groups]])
      render(conn, "show.html", organisation: organisation)
    end
  end

  def edit(conn, %{"id" => id}) do
    organisation = Repo.get!(Organisation, id)
    changeset = Organisation.changeset(organisation)
    render(conn, "edit.html", organisation: organisation, changeset: changeset)
  end

  def update(conn, %{"id" => id, "organisation" => organisation_params}) do
    organisation = Repo.get!(Organisation, id)
    changeset = Organisation.changeset(organisation, organisation_params)

    case Repo.update(changeset) do
      {:ok, organisation} ->
        AuditLog.log(conn, "Updated organisation", name: organisation.name)
        conn
        |> put_flash(:info, "Organisation updated successfully.")
        |> redirect(to: admin_organisation_path(conn, :show, organisation))
      {:error, changeset} ->
        render(conn, "edit.html", organisation: organisation, changeset: changeset)
    end
  end

  def delete(conn, %{"id" => id}) do
    organisation = Repo.get!(Organisation, id)

    # Here we use delete! (with a bang) because we expect
    # it to always work (and if it does not, it will raise).
    Repo.delete!(organisation)

    AuditLog.log(conn, "Removed organisation", name: organisation.name)
    conn
    |> put_flash(:info, "Organisation deleted successfully.")
    |> redirect(to: admin_organisation_path(conn, :index))
  end

  defp verify_org_permissions(conn, id) do
    user = conn.assigns.current_user
    if Air.User.admin?(user) or (user != nil and String.to_integer(id) == user.organisation_id) do
      conn
    else
      conn
      |> Phoenix.Controller.redirect(to: "/")
      |> Plug.Conn.halt()
    end
  end
end

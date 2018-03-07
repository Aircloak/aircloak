defmodule CentralWeb.LicenseController do
  @moduledoc false
  use Central.Web, :controller
  alias Central.{Service}

  plug :load_customer


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    render_index(conn, Service.License.empty_changeset())
  end

  def create(conn, %{"license" => params}) do
    case Service.License.create(conn.assigns.customer, params) do
      {:ok, _} ->
        conn
        |> put_flash(:info, "License created")
        |> redirect(to: customer_license_path(conn, :index, conn.assigns.customer.id))
      {:error, changeset} -> render_index(conn, changeset)
    end
  end

  def show(conn, %{"id" => id}) do
    case Service.License.export(conn.assigns.customer, id) do
      {:ok, result} -> send_resp(conn, 200, result)
      :not_found -> not_found(conn)
    end
  end

  def edit(conn, %{"id" => id}) do
    case Service.License.get(conn.assigns.customer, id) do
      {:ok, license} -> render(conn, "edit.html", license_id: id, changeset: Service.License.empty_changeset(license))
      :not_found -> not_found(conn)
    end
  end

  def update(conn, %{"id" => id, "license" => params}) do
    with {:ok, license} <- Service.License.get(conn.assigns.customer, id), \
      {:ok, _} <- Service.License.update(license, params)
    do
      conn
      |> put_flash(:info, "License updated")
      |> redirect(to: customer_license_path(conn, :index, conn.assigns.customer.id))
    else
      :not_found -> not_found(conn)
      {:error, changeset} -> render(conn, "edit.html", license_id: id, changeset: changeset)
    end
  end

  def revoke(conn, %{"license_id" => id}) do
    with {:ok, license} <- Service.License.get(conn.assigns.customer, id) do
      {:ok, _} = Service.License.revoke(license)

      conn
      |> put_flash(:info, "License revoked")
      |> redirect(to: customer_license_path(conn, :index, conn.assigns.customer.id))
    else
      :not_found -> not_found(conn)
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp render_index(conn, license_changeset) do
    customer = conn.assigns.customer
    licenses =
      Service.License.for_customer(customer)
      |> Enum.sort_by(fn(license) -> {license.revoked, license.inserted_at} end)

    render(conn, "index.html", changeset: license_changeset, customer: customer, licenses: licenses)
  end

  defp load_customer(conn, _) do
    case Service.Customer.get(conn.params["customer_id"]) do
      {:error, :not_found} -> not_found(conn)
      {:ok, customer} -> assign(conn, :customer, customer)
    end
  end
end

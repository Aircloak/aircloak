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
      {:ok, _} -> redirect_to_index(conn, "License created")
      {:error, changeset} -> render_index(conn, changeset)
    end
  end

  def show(conn, %{"id" => id}) do
    with_license(conn, id, fn(license) ->
      send_resp(conn, 200, Service.License.export(license))
    end)
  end

  def edit(conn, %{"id" => id}) do
    with_license(conn, id, fn(license) ->
      render(conn, "edit.html", license_id: id, changeset: Service.License.empty_changeset(license))
    end)
  end

  def update(conn, %{"id" => id, "license" => params}) do
    with_license(conn, id, fn(license) ->
      case Service.License.update(license, params) do
        {:error, changeset} -> render(conn, "edit.html", license_id: id, changeset: changeset)
        {:ok, _} -> redirect_to_index(conn, "License updated")
      end
    end)
  end

  def revoke(conn, %{"license_id" => id}) do
    with_license(conn, id, fn(license) ->
      {:ok, _} = Service.License.revoke(license)
      redirect_to_index(conn, "License revoked")
    end)
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  def with_license(conn, id, processor) do
    with {:ok, license} <- Service.License.get(conn.assigns.customer, id) do
      processor.(license)
    else
      :not_found -> not_found(conn)
    end
  end

  def redirect_to_index(conn, flash) do
    conn
    |> put_flash(:info, flash)
    |> redirect(to: customer_license_path(conn, :index, conn.assigns.customer.id))
  end

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

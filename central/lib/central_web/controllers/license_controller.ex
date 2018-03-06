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

  def create(conn, %{"license" => %{"name" => name, "length_in_days" => length_in_days, "auto_renew" => auto_renew}}) do
    Service.License.create(conn.assigns.customer, %{
      name: name,
      length_in_days: length_in_days,
      auto_renew: auto_renew,
    })
    |> case do
      {:ok, _} -> redirect(conn, to: customer_license_path(conn, :index, conn.assigns.customer.id))
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
      {:ok, license} -> render(conn, "edit.html", changeset: Service.License.empty_changeset(license))
      :not_found -> not_found(conn)
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp render_index(conn, license_changeset) do
    customer = conn.assigns.customer
    licenses =  Service.License.for_customer(customer)

    render(conn, "index.html", changeset: license_changeset, customer: customer, licenses: licenses)
  end

  defp load_customer(conn, _) do
    case Service.Customer.get(conn.params["customer_id"]) do
      {:error, :not_found} -> not_found(conn)
      {:ok, customer} -> assign(conn, :customer, customer)
    end
  end
end

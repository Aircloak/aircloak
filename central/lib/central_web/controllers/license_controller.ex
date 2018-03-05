defmodule CentralWeb.LicenseController do
  @moduledoc false
  use Central.Web, :controller
  alias Central.{Service}

  plug :load_customer


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    customer = conn.assigns.customer
    licenses =  Service.License.for_customer(customer)

    render(conn, "index.html", customer: customer, licenses: licenses)
  end

  def create(conn, %{"license" => %{"name" => name}}) do
    Service.License.create(conn.assigns.customer, %{name: name})
    redirect(conn, to: customer_license_path(conn, :index, conn.assigns.customer.id))
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp load_customer(conn, _) do
    case Service.Customer.get(conn.params["customer_id"]) do
      {:error, :not_found} -> not_found(conn)
      {:ok, customer} -> assign(conn, :customer, customer)
    end
  end
end

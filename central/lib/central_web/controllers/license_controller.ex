defmodule CentralWeb.LicenseController do
  @moduledoc false
  use Central.Web, :controller
  alias Central.{Service}

  plug :load_customer


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    render(conn, "index.html", customer: conn.assigns.customer)
  end

  def create(conn, _params) do
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

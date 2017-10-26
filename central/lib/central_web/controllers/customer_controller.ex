defmodule Central.CustomerController do
  @moduledoc false
  use Central.Web, :controller

  alias Central.{Schemas, Service}

  plug :load_customer when action in [:show, :edit, :update, :delete, :token]


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    render(conn, "index.html", customers: Service.Customer.all())
  end

  def new(conn, _params) do
    render(conn, "new.html", changeset: Schemas.Customer.empty_changeset())
  end

  def token(conn, _params) do
    customer = conn.assigns.customer
    {:ok, token} = Service.Customer.generate_token(customer)
    render(conn, "token.html", customer: customer, token: token)
  end

  def show(conn, _params) do
    render(conn, "show.html", customer: conn.assigns.customer)
  end

  def edit(conn, _params) do
    customer = conn.assigns.customer
    render(conn, "edit.html", changeset: Schemas.Customer.changeset(customer), customer: customer)
  end

  def create(conn, params) do
    case Service.Customer.create(params["customer"]) do
      {:ok, _customer} ->
        conn
        |> put_flash(:info, "Customer created")
        |> redirect(to: customer_path(conn, :index))
      {:error, changeset} -> render(conn, "new.html", changeset: changeset)
    end
  end

  def update(conn, params) do
    case Service.Customer.update(conn.assigns.customer, params["customer"]) do
      {:ok, _customer} ->
        conn
        |> put_flash(:info, "Customer updated")
        |> redirect(to: customer_path(conn, :index))
      {:error, changeset} -> render(conn, "edit.html", changeset: changeset)
    end
  end

  def delete(conn, _params) do
    case Service.Customer.delete(conn.assigns.customer) do
      :ok ->
        conn
        |> put_flash(:info, "Customer deleted")
        |> redirect(to: customer_path(conn, :index))
      :error ->
        conn
        |> put_flash(:error, "Could not delete the customer")
        |> redirect(to: customer_path(conn, :index))
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp load_customer(conn, _) do
    id = conn.params["id"] || conn.params["customer_id"]
    case Service.Customer.get(id) do
      {:error, :not_found} ->
        conn
        |> put_layout(false)
        |> put_status(:not_found)
        |> put_view(Central.ErrorView)
        |> render("404.html")
        |> halt()
      {:ok, customer} -> assign(conn, :customer, customer)
    end
  end
end

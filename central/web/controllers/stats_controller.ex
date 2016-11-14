defmodule Central.StatsController do
  @moduledoc false
  use Central.Web, :controller

  alias Central.Service.{Stats, Customer}

  plug :set_layout


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    dash_url = Central.site_setting("kibana_main_dash")
    render(conn, "index.html", dash: dash_url)
  end

  def show(conn, %{"id" => id}) do
    {:ok, customer} = Customer.get(id)
    render(conn, "show.html", customer: customer, stats: Stats.for_customer(customer))
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp set_layout(conn, _) do
    conn
    |> put_layout("fullscreen.html")
  end
end

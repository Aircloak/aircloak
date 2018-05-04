defmodule AirWeb.ResetPasswordController do
  @moduledoc false
  use Air.Web, :controller

  # -------------------------------------------------------------------
  # AirWeb.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions, do: %{anonymous: [:show, :update]}

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def show(conn, _params), do: render(conn, "show.html")
end

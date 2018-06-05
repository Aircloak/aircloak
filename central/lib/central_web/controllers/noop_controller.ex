defmodule CentralWeb.NoopController do
  @moduledoc false
  use Central.Web, :controller

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def get(conn, _params), do: text(conn, "OK")
end

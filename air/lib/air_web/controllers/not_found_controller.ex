defmodule AirWeb.NotFoundController do
  @moduledoc false

  use Air.Web, :controller

  def permissions do
    %{
      anonymous: :all,
      user: :all,
      admin: :all
    }
  end

  def index(conn, _params), do: not_found(conn)
end

defmodule AirWeb.PermalinkController do
  @moduledoc false
  use Air.Web, :controller

  alias Air.Service.{Token, Query}

  # -------------------------------------------------------------------
  # AirWeb.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{
      anonymous: :all,
      user: :all
    }
  end

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def query(conn, params) do
    with {:ok, query} <- Token.query_from_token(conn.assigns.current_user, params["token"]) do
      render(conn, "query.html",
        query: for_display(query, Query.buckets(query, 0)),
        csrf_token: Plug.CSRFProtection.get_csrf_token(),
        number_format: Air.Service.User.number_format_settings(conn.assigns.current_user)
      )
    else
      :error -> not_found(conn)
    end
  end

  # -------------------------------------------------------------------
  # Private functions
  # -------------------------------------------------------------------

  defp for_display(query, results),
    do: query |> Query.for_display(results) |> Map.drop([:private_permalink, :public_permalink])
end

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
    case Token.query_from_token(conn.assigns.current_user, params["token"]) do
      {:ok, query} -> render(conn, "query.html", query: for_display(query))
      :error -> not_found(conn)
    end
  end

  # -------------------------------------------------------------------
  # Private functions
  # -------------------------------------------------------------------

  defp for_display(query), do: query |> Query.for_display() |> Map.drop([:private_permalink, :public_permalink])
end

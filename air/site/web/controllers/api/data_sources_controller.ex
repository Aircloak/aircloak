defmodule Air.API.DataSourcesController do
  @moduledoc false
  use Air.Web, :controller

  alias Air.{CloakInfo, Token}

  def permissions do
    %{user: :all}
  end

  def index(conn, _params) do
    json(conn, data_sources(conn.assigns.current_user))
  end

  defp data_sources(user) do
    for cloak <- CloakInfo.all(user.organisation),
        data_source <- cloak.data_sources
    do
      %{
        name: "#{data_source.id} #{cloak.name}",
        token: Token.data_source_token(cloak.id, data_source.id)
      }
    end
  end
end

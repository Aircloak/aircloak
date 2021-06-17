defmodule AirWeb.Admin.SystemStatusView do
  @moduledoc false
  use Air.Web, :view

  alias Air.Repo

  def format_queries(queries),
    do:
      queries
      |> Repo.preload([:user, :data_source])
      |> Enum.map(fn query ->
        %{
          id: query.id,
          analyst_name: query.user.name,
          data_source_name: query.data_source.name,
          cloak_name: query.cloak_id,
          state: query.query_state,
          statement: query.statement
        }
      end)
end

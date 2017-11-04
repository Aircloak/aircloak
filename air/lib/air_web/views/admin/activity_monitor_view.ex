defmodule AirWeb.Admin.ActivityMonitorView do
  @moduledoc false;
  use Air.Web, :view

  alias Air.Repo

  def format_queries(queries), do:
    queries
    |> Repo.preload([:user, :data_source])
    |> Enum.map(fn(query) ->
      %{
        id: query.id,
        analyst_name: query.user.name,
        data_source_name: query.data_source.name,
        cloak_name: query.cloak_id,
        state: query.query_state,
        statement: query.statement,
      }
    end)

  def format_cloaks(cloaks), do:
    Enum.map(cloaks, &format_cloak/1)

  def format_cloak(cloak_info, fresh_memory_reading), do:
    format_cloak(Map.put(cloak_info, :memory, fresh_memory_reading))

  def format_cloak(cloak_info), do:
    Map.merge(cloak_info, %{
      total_memory: Map.get(cloak_info[:memory], :total_memory, nil),
      available_memory: Map.get(cloak_info[:memory], :available_memory, %{}),
    })
end

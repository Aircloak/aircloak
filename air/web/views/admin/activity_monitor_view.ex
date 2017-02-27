defmodule Air.Admin.ActivityMonitorView do
  @moduledoc false;
  use Air.Web, :view
  # bug in the current Phoenix
  @dialyzer :no_match

  alias Air.Repo

  def format_queries(queries), do:
    queries
    |> Repo.preload([:user, :data_source])
    |> Enum.map(fn(query) ->
      %{
        id: query.id,
        analyst_name: query.user.name,
        data_source_name: query.data_source.name,
        state: query.query_state,
        statement: query.statement,
      }
    end)

  def format_cloaks(cloaks), do:
    Enum.map(cloaks, &format_cloak/1)

  def format_cloak(cloak, reading \\ %{total_memory: nil, free_memory: nil}), do:
    Map.merge(reading, Map.take(cloak, [:id, :name]))
end

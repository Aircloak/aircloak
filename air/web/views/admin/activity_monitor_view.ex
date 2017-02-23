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
        state: query_state(query),
        statement: query.statement,
      }
    end)

  defp query_state(query) do
    if query.completed do
      "completed"
    else
      "started"
    end
  end
end

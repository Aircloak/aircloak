defmodule Air.PsqlServer.SpecialQueries.Tableau do
  @moduledoc "Handles common special queries issued by Tableau."

  @behaviour Air.PsqlServer.SpecialQueries

  # -------------------------------------------------------------------
  # SpecialQueries callback functions
  # -------------------------------------------------------------------

  @impl SpecialQueries
  def run_query(_conn, _query), do: nil

  @impl SpecialQueries
  def describe_query(_conn, _query, _params), do: nil
end

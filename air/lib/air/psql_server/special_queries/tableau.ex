defmodule Air.PsqlServer.SpecialQueries.Tableau do
  @moduledoc "Handles common special queries issued by Tableau."
  @behaviour Air.PsqlServer.SpecialQueries

  alias Air.PsqlServer
  alias Air.PsqlServer.RanchServer


  #-----------------------------------------------------------------------------------------------------------
  # SpecialQueries callback functions
  #-----------------------------------------------------------------------------------------------------------

  @doc false
  def handle_query(conn, query) do
    if query =~ ~r/begin;declare.* for select relname, nspname, relkind from.*fetch.*/i, do:
      fetch_tables(conn)
  end


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp fetch_tables(conn) do
    PsqlServer.start_async_query(conn, "show tables", [],
      fn(conn, {:ok, show_tables_response}) ->
        table_names =
          show_tables_response
          |> Map.fetch!("rows")
          |> Enum.map(fn(%{"row" => [table_name]}) -> table_name end)

        conn
        |> RanchServer.set_query_result(command: :begin, intermediate: true)
        |> RanchServer.set_query_result(command: :"declare cursor", intermediate: true)
        |> RanchServer.set_query_result(tables_list_for_tableau(table_names))
      end
    )
  end

  defp tables_list_for_tableau(table_names), do:
    [
      command: :fetch,
      columns:
        [
          %{name: "relname", type: :name},
          %{name: "nspname", type: :name},
          %{name: "relkind", type: :char},
        ],
      rows: Enum.map(table_names, &[&1, "public", ?r])
    ]
end

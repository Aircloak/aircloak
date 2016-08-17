defmodule Air.DataSource do
  @moduledoc "Functions for dealing with data sources without caring about cloaks."

  alias Air.{CloakInfo, Repo, Organisation, Query}

  @type data_source :: %{
    id: String.t,
    # Keep token for backwards compatibility for old API's until they are updated.
    # Will be removed.
    token: String.t,
    name: String.t,
    tables: [CloakInfo.table],
    cloak_id: CloakInfo.id,
    cloak_name: String.t,
  }


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns a flat list of all data sources available to the given organisation."
  @spec all(Plug.Conn.t) :: [data_source]
  def all(conn) do
    organisation = Repo.get!(Organisation, conn.assigns.current_user.organisation_id)
    for cloak <- CloakInfo.all(organisation),
      data_source <- cloak.data_sources
    do
      %{
        id: "#{data_source.id}@#{cloak.name}",
        # For backwards compatibility, we keep token in the structure,
        # so old API clients keep on working
        token: "#{data_source.id}@#{cloak.name}",
        name: data_source.id,
        tables: data_source.tables,
        cloak_id: cloak.id,
        cloak_name: cloak.name,
      }
    end
  end

  @doc "Returns a data source given it's ID, or nil if it cannot be found"
  @spec by_id(Plug.Conn.t, String.t) :: data_source | nil
  def by_id(conn, id) do
    [data_source_name, cloak_name] = String.split(id, "@", parts: 2)
    Enum.find(all(conn), fn
      (%{name: ^data_source_name, cloak_name: ^cloak_name}) -> true
      (_) -> false
    end)
  end

  @doc "Returns the data source that was queried last. Nil if none is found"
  @spec latest_data_source(Plug.Conn.t) :: data_source | nil
  def latest_data_source(conn) do
    queries = Query
    |> Query.for_user(conn.assigns.current_user)
    |> Query.recent(10)
    |> Repo.all()

    data_sources = all(conn)

    Enum.find_value(queries, fn(query) ->
      Enum.find_value(data_sources, fn(data_source) ->
        cond do
          query.cloak_id == data_source.cloak_id and query.data_source == data_source.name -> data_source
          true -> nil
        end
      end)
    end)
  end
end

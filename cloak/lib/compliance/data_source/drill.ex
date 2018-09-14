defmodule Compliance.DataSource.Drill do
  @moduledoc false

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  use Compliance.DataSource.Connector
  alias Compliance.DataSource.{Connector, MongoDB}

  @impl Connector
  def setup(%{parameters: params}) do
    Application.ensure_all_started(:httpoison)

    port = Map.get(params, :management_port, 8047)
    mongo_port = Map.get(params.mongo, :port, 27017)
    Connector.await_port(params.hostname, port)
    configure_mongo_in_drill(params.hostname, port, params.mongo.hostname, mongo_port)

    MongoDB.setup(%{parameters: params.mongo})
  end

  @impl Connector
  def connect(%{parameters: params}), do: MongoDB.connect(%{parameters: params.mongo})

  @impl Connector
  defdelegate create_table(table_name, columns, conn), to: MongoDB

  @impl Connector
  defdelegate after_tables_created(state), to: MongoDB

  @impl Connector
  defdelegate insert_rows(table_name, data, conn), to: MongoDB

  @impl Connector
  defdelegate insert_documents(collection_name, documents, conn), to: MongoDB

  @impl Connector
  defdelegate terminate(conn), to: MongoDB

  defp configure_mongo_in_drill(drill_hostname, drill_port, mongo_hostname, mongo_port) do
    config =
      Poison.encode!(%{
        type: "mongo",
        connection: "mongodb://#{mongo_hostname}:#{mongo_port}",
        enabled: true
      })

    data = "name=mongo&config=#{URI.encode_www_form(config)}"

    HTTPoison.post!("http://#{drill_hostname}:#{drill_port}/storage/mongo", data, [
      {"Content-Type", "application/x-www-form-urlencoded"}
    ])
  end
end

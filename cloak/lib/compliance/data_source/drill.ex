defmodule Compliance.DataSource.Drill do
  @moduledoc false

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  use Compliance.DataSource.Connector
  alias Compliance.DataSource.{Connector, MongoDB}

  @impl Connector
  def setup(%{parameters: params}) do
    Connector.await_port(params.hostname, Map.get(params, :port, 8047))
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
end

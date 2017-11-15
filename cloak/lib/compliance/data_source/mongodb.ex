defmodule Compliance.DataSource.MongoDB do
  @moduledoc false

  use Lens.Macros


  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  use Compliance.DataSource.Connector
  alias Compliance.DataSource.Connector

  @impl Connector
  def setup(%{parameters: params}) do
    Application.ensure_all_started(:mongodb)
    Connector.await_port(params.hostname, 27017)
    {:ok, conn} = Mongo.start_link(
      database: params.database,
      hostname: params.hostname,
      username: params.username
    )
    conn
  end

  @impl Connector
  def create_table(table_name, _columns, conn) do
    Mongo.delete_many!(conn, table_name, %{})
    conn
  end

  @impl Connector
  def insert_rows(_table_name, _data, conn), do: conn

  @impl Connector
  def insert_documents(collection_name, documents, conn) do
    converted_documents = convert_documents(documents)
    Mongo.insert_many!(conn, collection_name, converted_documents)
    conn
  end

  @impl Connector
  def terminate(_conn), do: :ok


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  def convert_documents(documents), do:
    Lens.map(dates_lens(), documents, &DateTime.from_naive!(&1, "Etc/UTC"))

  deflens dates_lens() do
    Lens.match(fn
      (%NaiveDateTime{}) -> Lens.root()
      (values) when is_list(values) -> Lens.all() |> dates_lens()
      (val) when is_map(val) -> Lens.map_values() |> dates_lens()
      (_other) -> Lens.empty()
    end)
  end
end

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
    Connector.await_port(params.hostname, Map.get(params, :port, 27_017))
  end

  @impl Connector
  def connect(%{parameters: params}) do
    {:ok, conn} =
      Mongo.start_link(
        database: params.database,
        hostname: params.hostname,
        port: Map.get(params, :port, 27_017),
        username: params.username,
        sync_connect: true,
        pool: DBConnection.Connection
      )

    conn
  end

  @impl Connector
  def create_table(table_name, _columns, conn) do
    Mongo.delete_many!(conn, table_name, %{})
    conn
  end

  @impl Connector
  def after_tables_created(state), do: state

  @impl Connector
  def insert_rows(collection_name, data, conn) do
    Mongo.insert_many!(conn, collection_name, data)
    conn
  end

  @impl Connector
  def prepare_data(data), do: data |> Compliance.Data.to_collections() |> convert_data()

  @impl Connector
  def terminate(_conn), do: :ok

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  def convert_data(data) do
    Lens.map(dates_lens(), data, fn
      date = %NaiveDateTime{} -> DateTime.from_naive!(date, "Etc/UTC")
      date = %Date{} -> {Date.to_erl(date), {0, 0, 0}} |> NaiveDateTime.from_erl!() |> DateTime.from_naive!("Etc/UTC")
    end)
  end

  deflens dates_lens() do
    Lens.match(fn
      %Date{} -> Lens.root()
      %NaiveDateTime{} -> Lens.root()
      values when is_list(values) -> Lens.all() |> dates_lens()
      val when is_map(val) -> Lens.map_values() |> dates_lens()
      _other -> Lens.empty()
    end)
  end
end

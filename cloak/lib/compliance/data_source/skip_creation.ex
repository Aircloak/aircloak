defmodule Compliance.DataSource.SkipCreation do
  @moduledoc false

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  use Compliance.DataSource.Connector
  alias Compliance.DataSource.Connector

  @impl Connector
  def setup(_), do: :ok

  @impl Connector
  def connect(_), do: nil

  @impl Connector
  def create_table(_table_name, _columns, conn), do: conn

  @impl Connector
  def after_tables_created(state), do: state

  @impl Connector
  def insert_rows(_table_name, _data, conn), do: conn

  @impl Connector
  def terminate(_conn), do: :ok
end

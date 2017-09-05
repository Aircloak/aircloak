defmodule Compliance.DataSource.Connector do
  @moduledoc "Specifies the interface needed to create tables and insert data for a test dataset"

  @type state :: any

  @doc "Establishes a connection to the database and does other necessary setup"
  @callback setup(Cloak.DataSource.t) :: state

  @doc "Is expected to create the necessary tables for the test dataset"
  @callback create_table(String.t, [{String.t, Compliance.TableDefinitions.column_type}], state) :: state

  @doc "Is expected to insert the provided data into the data source"
  @callback insert_rows(String.t, [Map.t], state) :: state

  @doc "Is supposed to do any cleanup if required"
  @callback terminate(state) :: :ok
end

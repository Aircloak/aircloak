defmodule Cloak.AnalystTable do
  @moduledoc "Functions for working with analyst tables."

  @enforce_keys [:id, :data_source, :query, :statement]
  defstruct [:id, :data_source, :query, :statement]

  @type t :: %__MODULE__{id: id, data_source: Cloak.DataSource.t(), query: Cloak.Sql.Query.t(), statement: String.t()}
  @type id :: any

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Creates the structure representing the analyst table."
  @spec new(id, String.t(), Cloak.DataSource.t()) :: {:ok, t} | {:error, String.t()}
  def new(id, statement, data_source) do
    with :ok <- supports_analyst_tables?(data_source),
         {:ok, query} <- compile_statement(statement, data_source),
         :ok <- verify_query_type(query),
         :ok <- verify_offloading(query),
         do: {:ok, %__MODULE__{id: id, data_source: data_source, query: query, statement: statement}}
  end

  @doc """
  Computes the database table name.

  The table name is derived from the table id and the generated sql. If any of these pieces of data changes, the table
  name will be different. This helps us remove various conflicting situations such as:

  - the new version of the cloak generates a different SQL
  - the table has been modified while a previous query is running
  - shields us from possible attacks (see https://github.com/Aircloak/aircloak/issues/3467#issuecomment-455563526 for
    example)

  Example:

  ```
  iex> AnalystTable.name(table!(1, "select user_id, x from mv1"))
  "__ac_2tiqXfcCKW9iKREwe5aLfUFgf"
  ```
  """
  @spec name(t) :: String.t()
  def name(table) do
    hash = :crypto.hash(:sha256, :erlang.term_to_binary([table.id, table.data_source.driver.db_query(table.query)]))
    encoded_hash = Base.encode64(hash, padding: false)

    # make sure the name is not longer than 30 characters to avoid possible issues with some databases, such as Oracle
    String.slice("__ac_#{encoded_hash}", 0, 30)
  end

  @doc "Stores the analyst table to database."
  @spec store(t) :: :ok | {:error, String.t()}
  def store(table) do
    Cloak.DataSource.Connection.execute!(
      table.data_source,
      fn connection ->
        table_name = name(table)

        if Enum.any?(table.data_source.driver.analyst_tables(connection), &(&1 == table_name)),
          do: :ok,
          else: table.data_source.driver.store_analyst_table(connection, table_name, table.query)
      end
    )
  end

  @doc "Returns the names of all stored analyst tables."
  @spec stored(Cloak.DataSource.t()) :: [String.t()]
  def stored(data_source),
    do: Cloak.DataSource.Connection.execute!(data_source, &data_source.driver.analyst_tables/1)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp supports_analyst_tables?(data_source) do
    if data_source.driver.supports_analyst_tables?(),
      do: :ok,
      else: {:error, "This data source doesn't support analyst tables."}
  end

  defp compile_statement(statement, data_source) do
    with {:ok, parsed_query} <- Cloak.Sql.Parser.parse(statement),
         {:ok, query} <- Cloak.Sql.Compiler.compile_direct(parsed_query, data_source),
         do: {:ok, query |> Cloak.Sql.Query.set_emulation_flag() |> Cloak.Sql.Compiler.Anonymization.set_query_type()}
  end

  defp verify_query_type(query) do
    if query.type == :restricted, do: :ok, else: {:error, "At least one user id column must be selected."}
  end

  defp verify_offloading(query) do
    if query.emulated?, do: {:error, "Emulated query can't be materialized."}, else: :ok
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(table, opts) do
      # reduce inspected data for easier debugging
      data = [id: table.id, data_source: table.data_source.name, statement: table.statement]
      concat(["#Cloak.AnalystTable<", to_doc(data, opts), ">"])
    end
  end
end

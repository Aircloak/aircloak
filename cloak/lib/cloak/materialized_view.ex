defmodule Cloak.MaterializedView do
  @enforce_keys [:id, :data_source, :query]
  defstruct [:id, :data_source, :query]

  def new(id, statement, data_source) do
    with :ok <- supports_materialized_views?(data_source),
         {:ok, query} <- compile_statement(statement, data_source),
         :ok <- verify_query_type(query),
         :ok <- verify_offloading(query),
         do: {:ok, %__MODULE__{id: id, data_source: data_source, query: query}}
  end

  @doc """
  Computes the view name which can be used as the table name in the database.

  The view name is derived from the view id and the generated sql. If any of these pieces of data changes, the view
  name will be different. This helps us remove various conflicting situations such as:

  - the new version of the cloak generates a different SQL
  - the view has been modified while a previous query is running
  - shields us from possible attacks (see https://github.com/Aircloak/aircloak/issues/3467#issuecomment-455563526 for
    example)

  Example:

  ```
  iex> MaterializedView.name(view!(1, "select user_id, x from mv1"))
  "__ac_2tiqXfcCKW9iKREwe5aLfUFgf"
  ```
  """
  def name(view) do
    hash = :crypto.hash(:sha256, :erlang.term_to_binary([view.id, view.data_source.driver.db_query(view.query)]))
    encoded_hash = Base.encode64(hash, padding: false)

    # make sure the name is not longer than 30 characters to avoid possible issues with some databases, such as Oracle
    String.slice("__ac_#{encoded_hash}", 0, 30)
  end

  defp supports_materialized_views?(data_source) do
    if data_source.driver.supports_materialized_views?(),
      do: :ok,
      else: {:error, "This data source doesn't support materialized views."}
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
end

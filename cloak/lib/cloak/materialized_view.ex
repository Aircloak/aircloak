defmodule Cloak.MaterializedView do
  @enforce_keys [:id, :data_source, :db_select]
  defstruct [:id, :data_source, :db_select]

  def new(id, statement, data_source) do
    with :ok <- supports_materialized_views?(data_source),
         {:ok, query} <- compile_statement(statement, data_source),
         :ok <- verify_query_type(query),
         :ok <- verify_offloading(query),
         do: {:ok, %__MODULE__{id: id, data_source: data_source, db_select: Cloak.DataSource.SqlBuilder.build(query)}}
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

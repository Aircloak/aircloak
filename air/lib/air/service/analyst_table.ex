defmodule Air.Service.AnalystTable do
  @moduledoc "Service module for working with analyst tables."

  alias Air.Service.DataSource

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @spec create(Air.Schemas.User.t(), DataSource.t(), String.t(), String.t()) :: :ok | {:error, atom | String.t()}
  def create(user, data_source, name, sql) do
    DataSource.store_analyst_table({:id, data_source.id}, user, table_data(user, data_source, name, sql))
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp table_data(user, data_source, name, sql),
    do: %{analyst_id: user.id, table_name: name, statement: sql, data_source: data_source.name}
end

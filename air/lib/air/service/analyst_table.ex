defmodule Air.Service.AnalystTable do
  @moduledoc "Service module for working with analyst tables."

  alias Air.Service.DataSource
  alias Air.Schemas.AnalystTable
  alias Air.Repo

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Creates the new analyst table, and stores it in cloak and in air."
  @spec create(Air.Schemas.User.t(), DataSource.t(), String.t(), String.t()) :: :ok | {:error, atom | String.t()}
  def create(user, data_source, name, sql) do
    Repo.transaction(fn ->
      # We need to store locally first to check for duplicate table name. However, once we stored locally,
      # there's no guarantee that the table will be successfully stored in the cloak, so we're running this inside
      # a transaction.
      with {:ok, table} <- store_to_air(user, data_source, name, sql),
           :ok <- store_to_cloak(table, user, data_source) do
        table
      else
        {:error, changeset} -> Repo.rollback(changeset)
      end
    end)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp store_to_air(user, data_source, name, sql) do
    changes = %{data_source_id: data_source.id, user_id: user.id, name: name, sql: sql}

    %AnalystTable{}
    |> Ecto.Changeset.cast(changes, ~w(name sql user_id data_source_id)a)
    |> Ecto.Changeset.validate_required(~w(name sql user_id data_source_id)a)
    |> Ecto.Changeset.unique_constraint(:name, name: :analyst_tables_user_id_data_source_id_name_index)
    |> Repo.insert()
  end

  defp store_to_cloak(table, user, data_source) do
    case DataSource.store_analyst_table({:id, data_source.id}, user, table_data(user, data_source, table)) do
      :ok -> :ok
      {:error, reason} -> {:error, add_cloak_error(table, reason)}
    end
  end

  defp table_data(user, data_source, table),
    do: %{analyst_id: user.id, table_name: table.name, statement: table.sql, data_source: data_source.name}

  defp add_cloak_error(table, error),
    do: table |> Ecto.Changeset.change() |> Ecto.Changeset.add_error(:sql, cloak_error(error))

  defp cloak_error(:internal_error), do: "Internal error."
  defp cloak_error(:not_connected), do: not_connected_error()
  defp cloak_error(string) when is_binary(string), do: string

  defp not_connected_error() do
    "The table cannot be saved because no cloak is currently available for the given data source. " <>
      "Please contact your administrator."
  end
end

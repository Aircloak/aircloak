defmodule Air.Service.AnalystTable do
  @moduledoc "Service module for working with analyst tables."

  alias Air.Service.DataSource
  alias Air.Schemas.AnalystTable
  alias Air.Repo
  alias AirWeb.Socket.Cloak.MainChannel

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Creates the new analyst table, and stores it in cloak and in air."
  @spec create(Air.Schemas.User.t(), DataSource.t(), String.t(), String.t()) ::
          {:ok, AnalystTable.t()} | {:error, Ecto.ChangeSet.t()}
  def create(user, data_source, name, sql) do
    changes = %{data_source_id: data_source.id, user_id: user.id, name: name, sql: sql}

    %AnalystTable{}
    |> Ecto.Changeset.cast(changes, ~w(name sql user_id data_source_id)a)
    |> Ecto.Changeset.validate_required(~w(name sql user_id data_source_id)a)
    |> Map.put(:action, :insert)
    |> transactional_store(user, data_source)
  end

  @doc "Updates the existing analyst table, and stores it in cloak and in air."
  @spec update(pos_integer, String.t(), String.t()) :: {:ok, AnalystTable.t()} | {:error, Ecto.ChangeSet.t()}
  def update(table_id, name, sql) do
    table = AnalystTable |> Repo.get!(table_id) |> Repo.preload([:user, :data_source])

    table
    |> Ecto.Changeset.cast(%{name: name, sql: sql}, ~w(name sql)a)
    |> Ecto.Changeset.validate_required(~w(name sql user_id data_source_id)a)
    |> Map.put(:action, :update)
    |> transactional_store(table.user, table.data_source)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp transactional_store(changeset, user, data_source) do
    Repo.transaction(fn ->
      changeset =
        Ecto.Changeset.unique_constraint(
          changeset,
          :name,
          name: :analyst_tables_user_id_data_source_id_name_index
        )

      # We need to store locally first to check for duplicate table name. However, once we stored locally,
      # there's no guarantee that the table will be successfully stored in the cloak, so we're running this inside
      # a transaction.
      with {:ok, table} <- apply(Repo, changeset.action, [changeset]),
           {:ok, registration_info} <- store_to_cloak(table, user, data_source),
           # at this point we can update the table with the registration info obtaine from cloak
           {:ok, table} <- table |> Ecto.Changeset.change(%{registration_info: registration_info}) |> Repo.update() do
        table
      else
        {:error, changeset} -> Repo.rollback(changeset)
      end
    end)
  end

  defp store_to_cloak(table, user, data_source) do
    with {:error, reason} <-
           DataSource.with_available_cloak(
             data_source,
             user,
             &MainChannel.store_analyst_table(&1.channel_pid, user.id, table.name, table.sql, data_source.name)
           ),
         do: {:error, add_cloak_error(table, reason)}
  end

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

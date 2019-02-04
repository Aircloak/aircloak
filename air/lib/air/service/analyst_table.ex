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
    |> store_table(user, data_source)
  end

  @doc "Updates the existing analyst table, and stores it in cloak and in air."
  @spec update(pos_integer, User.t(), String.t(), String.t()) ::
          {:ok, AnalystTable.t()} | {:error, Ecto.ChangeSet.t() | :not_allowed}
  def update(table_id, user, name, sql) do
    table = AnalystTable |> Repo.get!(table_id) |> Repo.preload([:user, :data_source])

    if table.user_id == user.id do
      table
      |> Ecto.Changeset.cast(%{name: name, sql: sql}, ~w(name sql)a)
      |> Ecto.Changeset.validate_required(~w(name sql user_id data_source_id)a)
      |> Map.put(:action, :update)
      |> store_table(table.user, table.data_source)
    else
      {:error, :not_allowed}
    end
  end

  @doc "Returns all known analyst tables."
  @spec all() :: [AnalystTable.t()]
  def all(), do: Repo.all(AnalystTable)

  @doc "Returns the changeset representing an empty table."
  @spec new_changeset() :: Changeset.t()
  def new_changeset(), do: Ecto.Changeset.cast(%AnalystTable{}, %{}, [])

  @doc "Returns the changeset representing the table with the given id."
  @spec changeset(integer) :: Changeset.t()
  def changeset(table_id), do: Ecto.Changeset.cast(Repo.get!(AnalystTable, table_id), %{}, [])

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp store_table(changeset, user, data_source) do
    with {:ok, _table} = success <- transactional_store(changeset, user, data_source) do
      # transactional_store stored the view on a single cloak. At this point, we're notifying all connected cloaks
      # about the change. This ensures that all the cloaks have the table stored, including those which have connected
      # during the transactional store.
      Air.Service.Cloak.channel_pids(data_source.name)
      |> Stream.map(fn {pid, _cloak_info} -> pid end)
      |> Enum.each(&MainChannel.send_analyst_tables_to_cloak/1)

      success
    end
  end

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
           {:ok, result_info} <- store_to_cloak(table, user, data_source),
           # at this point we can update the table with the registration info obtaine from cloak
           {:ok, table} <-
             table |> Ecto.Changeset.change(%{registration_info: result_info.registration_info}) |> Repo.update() do
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

defmodule Air.Service.AnalystTable do
  @moduledoc "Service module for working with analyst tables."

  alias Air.Service.{DataSource, User}
  alias Air.Schemas.AnalystTable
  alias Air.Repo
  alias AirWeb.Socket.Cloak.MainChannel
  import Ecto.Query

  @creation_status_supervisor __MODULE__.CreationStatusSupervisor

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

  @doc """
  Updates the creation status for an analyst table in the air.

  Returns immediately with an error if the status is invalid, and otherwise returns :ok.
  The state update process happens asynchronously in the background to unblock the caller.
  No feedback will be given with respect to whether the operation succeeded or not
  beyond whether the desired state is an allowed one.
  """
  @spec update_status(non_neg_integer, String.t(), String.t(), AnalystTable.CreationStatus.t()) ::
          :ok | {:error, :invalid_status}
  def update_status(_user_id, _data_source_name, _table_name, status)
      when status not in [:pending, :succeeded, :failed],
      do: {:error, :invalid_status}

  def update_status(user_id, data_source_name, table_name, status) do
    Task.Supervisor.start_child(@creation_status_supervisor, fn ->
      with {:ok, user} <- User.load(user_id),
           {:ok, data_source} <- DataSource.fetch_as_user({:name, data_source_name}, user),
           {:ok, analyst_table} <- get_by_name(user, data_source, table_name) do
        analyst_table
        |> Ecto.Changeset.cast(%{creation_status: status}, [:creation_status])
        |> Repo.update!()

        AirWeb.Socket.Frontend.UserChannel.broadcast_analyst_selectables_change(user, data_source)
      end
    end)

    :ok
  end

  @doc "Deletes the analyst table."
  @spec delete(pos_integer, User.t()) :: :ok | {:error, :not_allowed}
  def delete(table_id, user) do
    table = AnalystTable |> Repo.get!(table_id) |> Repo.preload([:user, :data_source])

    if table.user_id == user.id do
      Repo.delete!(table)

      Air.Service.Cloak.channel_pids(table.data_source.name)
      |> Stream.map(fn {pid, _cloak_info} -> pid end)
      |> Enum.each(&MainChannel.send_analyst_tables_to_cloak/1)

      :ok
    else
      {:error, :not_allowed}
    end
  end

  @doc "Returns all known analyst tables."
  @spec all() :: [AnalystTable.t()]
  def all(), do: Repo.all(AnalystTable)

  @doc "Returns all of an analysts analyst tables for a given data source"
  @spec all(User.t(), DataSource.t()) :: [AnalystTable.t()]
  def all(user, data_source),
    do: AnalystTable |> by_user_id(user.id) |> by_data_source_id(data_source.id) |> Repo.all()

  @doc "Returns all known analyst tables for a data source"
  @spec all_for_data_source(DataSource.t()) :: [AnalystTable.t()]
  def all_for_data_source(data_source),
    do: AnalystTable |> by_data_source_id(data_source.id) |> Repo.all() |> Repo.preload(:user)

  @doc "Returns an analyst table by name belonging to a given user"
  @spec get_by_name(User.t(), DataSource.t(), String.t()) :: {:ok, AnalystTable.t()} | {:error, :not_found}
  def get_by_name(user, data_source, name) do
    case AnalystTable |> by_user_id(user.id) |> by_data_source_id(data_source.id) |> Repo.get_by(name: name) do
      nil -> {:error, :not_found}
      table -> {:ok, table}
    end
  end

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
    with {:ok, _} = success <- transactional_store(changeset, user, data_source) do
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
          name: :user_selectables_user_id_data_source_id_name_index
        )

      # We need to store locally first to check for duplicate table name. However, once we stored locally,
      # there's no guarantee that the table will be successfully stored in the cloak, so we're running this inside
      # a transaction.
      with {:ok, table} <- apply(Repo, changeset.action, [changeset]),
           {:ok, result_info} <- create_or_update_on_cloak(table, user, data_source),
           # at this point we can update the table with the registration info obtaine from cloak
           result_info_changeset = Ecto.Changeset.change(table.result_info || %AnalystTable.ResultInfo{}, result_info),
           table_changeset =
             table
             |> Ecto.Changeset.change()
             |> Ecto.Changeset.put_embed(:result_info, result_info_changeset),
           {:ok, table} <- Repo.update(table_changeset) do
        table
      else
        {:error, error_changeset} ->
          # Through various transformations the error changeset does no longer
          # contain all the state of the original changeset. This is problematic
          # for consumers. We have to artificially recreate it.
          Repo.rollback(%{error_changeset | action: changeset.action, changes: changeset.changes})
      end
    end)
  end

  defp create_or_update_on_cloak(table, user, data_source) do
    with {:error, reason} <-
           DataSource.with_available_cloak(
             data_source,
             user,
             &MainChannel.create_or_update_analyst_table(
               &1.channel_pid,
               user.id,
               table.name,
               table.sql,
               data_source.name,
               nil,
               Air.Service.View.user_views_map(user, data_source.id)
             )
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

  defp by_user_id(scope, user_id), do: where(scope, [v], v.user_id == ^user_id)

  defp by_data_source_id(scope, data_source_id), do: where(scope, [v], v.data_source_id == ^data_source_id)

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_args),
    do:
      Aircloak.ChildSpec.supervisor(
        [
          {Task.Supervisor, name: @creation_status_supervisor}
        ],
        strategy: :one_for_one,
        name: __MODULE__
      )
end

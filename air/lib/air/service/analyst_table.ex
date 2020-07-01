defmodule Air.Service.AnalystTable do
  @moduledoc "Service module for working with analyst tables."

  alias Air.Service.{DataSource, User}
  alias Air.Schemas.AnalystTable
  alias Air.Repo
  alias AirWeb.Socket.Cloak.MainChannel
  import Ecto.Query

  @type user_analyst_tables :: %{
          String.t() => %{
            comment: String.t() | nil
          }
        }

  @creation_status_supervisor __MODULE__.CreationStatusSupervisor

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Creates the new analyst table, and stores it in cloak and in air."
  @spec create(Air.Schemas.User.t(), DataSource.t(), String.t(), String.t(), String.t() | nil) ::
          {:ok, AnalystTable.t()} | {:error, Ecto.ChangeSet.t()}
  def create(user, data_source, name, sql, comment) do
    changes = %{
      data_source_id: data_source.id,
      user_id: user.id,
      name: name,
      sql: sql,
      comment: comment
    }

    %AnalystTable{}
    |> Ecto.Changeset.cast(changes, ~w(name sql comment user_id data_source_id)a)
    |> Ecto.Changeset.validate_required(~w(name sql user_id data_source_id)a)
    |> Map.put(:action, :insert)
    |> transactional_store(nil, user, data_source)
  end

  @doc "Updates the existing analyst table, and stores it in cloak and in air."
  @spec update(pos_integer, User.t(), String.t(), String.t(), String.t() | nil) ::
          {:ok, AnalystTable.t()} | {:error, Ecto.ChangeSet.t() | :not_allowed}
  def update(table_id, user, name, sql, comment) do
    table = AnalystTable |> Repo.get!(table_id) |> Repo.preload([:user, :data_source])

    if table.user_id == user.id do
      table
      |> Ecto.Changeset.cast(
        %{name: name, sql: sql, comment: comment, creation_status: :pending},
        ~w(name sql comment creation_status)a
      )
      |> Ecto.Changeset.validate_required(~w(name sql user_id data_source_id)a)
      |> Map.put(:action, :update)
      |> transactional_store(table.name, table.user, table.data_source)
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
    table = Repo.get!(AnalystTable, table_id) |> Repo.preload([:user, :data_source])

    if table.user_id == user.id do
      with {:ok, validated_views} <-
             DataSource.with_available_cloak(
               table.data_source,
               table.user,
               &MainChannel.drop_analyst_table(
                 &1.channel_pid,
                 table.user.id,
                 table.name,
                 table.data_source.name,
                 Air.Service.View.user_views(user, table.data_source.id)
               )
             ) do
        Air.Service.View.store_view_validation_results(user, table.data_source.id, validated_views)
        Repo.delete!(table)
        sync_cloaks(table)

        :ok
      end
    else
      {:error, :not_allowed}
    end
  end

  @doc "Deletes all analyst tables of the given user."
  @spec delete_all(Air.Schemas.User.t()) :: :ok
  def delete_all(user) do
    Repo.preload(user, groups: :data_sources).groups
    |> Stream.flat_map(& &1.data_sources)
    |> Stream.uniq()
    |> Enum.each(&delete_all(user, &1))
  end

  @doc "Deletes all analyst tables of the given user in the given data source."
  @spec delete_all(Air.Schemas.User.t(), Air.Schemas.DataSource.t()) :: :ok
  def delete_all(user, data_source) do
    {_count, deleted_table_names} =
      Repo.delete_all(
        from(
          table in AnalystTable,
          where: table.user_id == ^user.id and table.data_source_id == ^data_source.id,
          select: table.name
        )
      )

    with [{channel_pid, _} | other_cloaks] <- Enum.shuffle(Air.Service.Cloak.channel_pids(data_source.name)) do
      MainChannel.drop_analyst_tables(channel_pid, user.id, data_source.name, deleted_table_names)
      Enum.each(other_cloaks, fn {pid, _cloak_info} -> MainChannel.refresh_analyst_tables(pid) end)
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

  @doc "Returns a map of all the analyst tables the given user defined for the given data source."
  @spec user_analyst_tables(User.t(), integer) :: user_analyst_tables
  def user_analyst_tables(user, data_source_id) do
    AnalystTable
    |> by_user_id(user.id)
    |> by_data_source_id(data_source_id)
    |> select([v], {v.name, %{comment: v.comment}})
    |> Repo.all()
    |> Enum.into(%{})
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

  defp transactional_store(changeset, old_table_name, user, data_source) do
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
           {:ok, {columns, validated_views}} <- create_or_update_on_cloak(table, old_table_name, user, data_source),
           :ok <- Air.Service.View.store_view_validation_results(user, data_source.id, validated_views),
           # at this point we can update the table with the registration info obtained from cloak
           table_changeset =
             table
             |> Ecto.Changeset.change()
             |> Ecto.Changeset.put_embed(:columns, columns),
           {:ok, table} <- Repo.update(table_changeset) do
        sync_cloaks(table)
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

  defp create_or_update_on_cloak(table, old_table_name, user, data_source) do
    with {:error, reason} <-
           DataSource.with_available_cloak(
             data_source,
             user,
             &MainChannel.create_or_update_analyst_table(
               &1.channel_pid,
               user.id,
               table.name,
               old_table_name,
               table.sql,
               data_source.name,
               nil,
               Air.Service.View.user_views(user, data_source.id)
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

  defp sync_cloaks(table) do
    table = Repo.preload(table, [:data_source])

    Enum.each(
      Air.Service.Cloak.channel_pids(table.data_source.name),
      fn {pid, _cloak_info} -> MainChannel.refresh_analyst_tables(pid) end
    )
  end

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

defmodule Air.Service.View do
  @moduledoc "Service module for working with views."

  alias Aircloak.ChildSpec
  alias Air.Schemas.{User, View}
  alias Air.{Repo, Service.DataSource}
  import Ecto.Query
  require Logger

  @type view_map :: %{optional(String.t()) => String.t()}
  @type view_metadata_map :: %{
          String.t() => %{
            comment: String.t() | nil
          }
        }

  @cloak_validations_sup __MODULE__.CloakValidationsSup
  @notifications_registry __MODULE__.NotificationsRegistry

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Subscribes to notifications about asynchronous activities, such as revalidations of views."
  @spec subscribe_to(:revalidated_views) :: :ok
  def subscribe_to(notification) do
    Registry.register(@notifications_registry, notification, nil)
    :ok
  end

  @doc "Unsubscribes from notifications about asynchronous activities, such as revalidations of views."
  @spec unsubscribe_from(:revalidated_views) :: :ok
  def unsubscribe_from(notification) do
    Registry.unregister(@notifications_registry, notification)
    :ok
  end

  @doc "Returns the changeset representing an empty view."
  @spec new_changeset() :: Changeset.t()
  def new_changeset(), do: Ecto.Changeset.cast(%View{}, %{}, [])

  @doc "Returns the changeset representing the view with the given id."
  @spec changeset(integer) :: Changeset.t()
  def changeset(view_id), do: Ecto.Changeset.cast(Repo.get!(View, view_id), %{}, [])

  @doc "Retrieves views of the given user for the given data source."
  @spec all(User.t(), DataSource.t()) :: [View.t()]
  def all(user, data_source), do: View |> by_user_id(user.id) |> by_data_source_id(data_source.id) |> Repo.all()

  def broken(user, data_source),
    do:
      View
      |> by_user_id(user.id)
      |> by_data_source_id(data_source.id)
      |> only_broken()
      |> Repo.all()

  @doc "Saves the new view in the database."
  @spec create(
          User.t(),
          DataSource.t(),
          String.t(),
          String.t(),
          String.t() | nil,
          revalidation_timeout: non_neg_integer,
          skip_revalidation: boolean
        ) :: {:ok, View.t()} | {:error, Ecto.Changeset.t()}
  def create(user, data_source, name, sql, comment, options \\ []) do
    changes = %{data_source_id: data_source.id, user_id: user.id, name: name, sql: sql, comment: comment}

    with {:ok, changeset} <- validated_view_changeset(%View{}, user, changes, :insert),
         {:ok, view} <- Repo.insert(changeset) do
      unless options[:skip_revalidation],
        do:
          revalidate_views_and_wait(
            user,
            view.data_source_id,
            Keyword.take(options, [:revalidation_timeout])
          )

      {:ok, view}
    end
  end

  @doc "Updates the existing view in the database."
  @spec update(
          integer,
          User.t(),
          String.t(),
          String.t(),
          String.t() | nil,
          revalidation_timeout: non_neg_integer
        ) ::
          {:ok, View.t()} | {:error, Ecto.Changeset.t()} | {:error, :not_allowed}
  def update(view_id, user, name, sql, comment, options \\ []) do
    view = Repo.get!(View, view_id)

    # view must be owned by the user
    if view.user_id == user.id do
      changes = %{name: name, sql: sql, comment: comment}

      with {:ok, changeset} <- validated_view_changeset(view, user, changes, :update) do
        {:ok, view} = Repo.update(changeset)

        revalidate_views_and_wait(
          user,
          view.data_source_id,
          Keyword.take(options, [:revalidation_timeout])
        )

        {:ok, view}
      end
    else
      {:error, :not_allowed}
    end
  end

  @doc "Deletes the given view from the database."
  @spec delete(integer, User.t(), revalidation_timeout: non_neg_integer) :: :ok | {:error, :not_allowed}
  def delete(view_id, user, options \\ []) do
    view = Repo.get!(View, view_id)

    if view.user_id == user.id do
      Repo.delete!(view)

      revalidate_views_and_wait(
        user,
        view.data_source_id,
        Keyword.take(options, [:revalidation_timeout])
      )

      :ok
    else
      {:error, :not_allowed}
    end
  end

  @doc "Delete all views into the given data source from the database."
  @spec delete_for_data_source(Air.Schemas.DataSource.t()) :: :ok
  def delete_for_data_source(data_source) do
    View |> by_data_source_id(data_source.id) |> Repo.delete_all()
    :ok
  end

  @doc "Returns a %{name => sql} map of all the views the given user defined for the given data source."
  @spec user_views_map(User.t(), integer) :: view_map
  def user_views_map(user, data_source_id) do
    View
    |> by_user_id(user.id)
    |> by_data_source_id(data_source_id)
    |> select([v], {v.name, v.sql})
    |> Repo.all()
    |> Enum.into(%{})
  end

  @doc "Returns a map containing metadata of all the views the given user defined for the given data source."
  @spec user_views_metadata(User.t(), integer) :: view_metadata_map
  def user_views_metadata(user, data_source_id) do
    View
    |> by_user_id(user.id)
    |> by_data_source_id(data_source_id)
    |> select([v], {v.name, %{comment: v.comment}})
    |> Repo.all()
    |> Enum.into(%{})
  end

  @doc "Asynchronously revalidates all views of all users having access to the given data source."
  @spec revalidate_all_views(Air.Schemas.DataSource.t()) :: :ok
  def revalidate_all_views(data_source),
    do:
      data_source
      |> DataSource.users()
      |> Enum.each(&revalidate_views(&1, data_source.id))

  @doc "Stores the validation results of user views and notifies subscribers."
  @spec store_view_validation_results(User.t(), integer, AirWeb.Socket.Cloak.MainChannel.validated_views()) :: :ok
  def store_view_validation_results(user, data_source_id, results) do
    for {name, result} <- results,
        {:ok, valid} <- [view_status(result)] do
      View
      |> by_data_source_id(data_source_id)
      |> by_user_id(user.id)
      |> Repo.get_by!(name: name)
      |> apply_view_changeset(%{broken: not valid})
      |> Repo.update()
    end

    notify_subscribers(:revalidated_views, %{user_id: user.id, data_source_id: data_source_id})

    :ok
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp notify_subscribers(notification, payload),
    do:
      Registry.lookup(@notifications_registry, notification)
      |> Enum.map(fn {pid, nil} -> pid end)
      |> Enum.each(&send(&1, {notification, payload}))

  defp revalidate_views_and_wait(user, data_source_id, options) do
    subscribe_to(:revalidated_views)

    try do
      revalidate_views(user, data_source_id)

      receive do
        {:revalidated_views, %{data_source_id: ^data_source_id}} -> :ok
      after
        Keyword.get(options, :revalidation_timeout, 0) ->
          {:error, :timeout}
      end
    after
      unsubscribe_from(:revalidated_views)
    end
  end

  defp revalidate_views(user, data_source_id),
    do: Task.Supervisor.start_child(@cloak_validations_sup, fn -> sync_revalidate_views!(user, data_source_id) end)

  defp sync_revalidate_views!(user, data_source_id) do
    Logger.info("revalidating views for user #{user.id}, data source #{data_source_id}")

    {:ok, results} = validate_views(data_source_id, user, user_views_map(user, data_source_id))
    store_view_validation_results(user, data_source_id, results)
  end

  defp validate_views(data_source_id, user, views) do
    DataSource.with_available_cloak(
      {:id, data_source_id},
      user,
      &{:ok, AirWeb.Socket.Cloak.MainChannel.validate_views(&1.channel_pid, user.id, &1.data_source.name, views)}
    )
  end

  defp view_status(validation_result) do
    case validation_result do
      {:ok, _} -> {:ok, true}
      {:error, :sql, _} -> {:ok, false}
      {:error, :name, _} -> {:ok, false}
      _ -> :error
    end
  end

  defp validated_view_changeset(view, user, changes, action) do
    changeset = %Ecto.Changeset{apply_view_changeset(view, changes) | action: action}

    if Enum.empty?(changeset.errors) do
      final_view = Ecto.Changeset.apply_changes(changeset)

      # make sure the user is correct
      true = final_view.user_id == user.id

      case validate_view(user, final_view) do
        {:ok, columns} ->
          {:ok, Ecto.Changeset.put_change(changeset, :columns, columns)}

        {:error, :name, name_error} ->
          # Name error returned by the cloak -> we'll convert into a changeset
          {:error, Ecto.Changeset.add_error(changeset, :name, name_error)}

        {:error, :sql, sql_error} ->
          # SQL error returned by the cloak -> we'll convert into a changeset
          {:error, Ecto.Changeset.add_error(changeset, :sql, sql_error)}

        {:error, :internal_error} ->
          {:error, Ecto.Changeset.add_error(changeset, :sql, "Internal error.")}

        {:error, :not_connected} ->
          # Cloak not available
          {:error,
           Ecto.Changeset.add_error(
             changeset,
             :sql,
             "The view cannot be saved because no cloak is currently available for the given data source. " <>
               "Please contact your administrator."
           )}
      end
    else
      {:error, changeset}
    end
  end

  defp validate_view(user, view) do
    views =
      user_views_map(user, view.data_source_id)
      |> Map.put(view.name, view.sql)

    case validate_views(view.data_source_id, user, views) do
      {:ok, results} -> Map.fetch!(results, view.name)
      error -> error
    end
  end

  defp apply_view_changeset(view, changes),
    do:
      view
      |> Ecto.Changeset.cast(changes, ~w(name sql comment user_id data_source_id broken)a)
      |> Ecto.Changeset.validate_required(~w(name sql user_id data_source_id)a)
      |> Ecto.Changeset.unique_constraint(:name, name: :user_selectables_user_id_data_source_id_name_index)

  defp by_user_id(scope, user_id), do: where(scope, [v], v.user_id == ^user_id)

  defp by_data_source_id(scope, data_source_id), do: where(scope, [v], v.data_source_id == ^data_source_id)

  defp only_broken(scope), do: where(scope, [v], v.broken == true)

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_arg) do
    ChildSpec.supervisor(
      [
        ChildSpec.task_supervisor(name: @cloak_validations_sup),
        ChildSpec.registry(:duplicate, @notifications_registry)
      ],
      strategy: :one_for_one,
      name: __MODULE__
    )
  end
end

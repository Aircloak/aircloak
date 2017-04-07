defmodule Air.Service.View do
  @moduledoc "Service module for working with views."

  alias Air.Schemas.{User, View}
  alias Air.{Repo, Service.DataSource, Version}
  import Ecto.Query


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns the changeset representing an empty view."
  @spec new_changeset() :: Changeset.t
  def new_changeset(), do:
    Ecto.Changeset.cast(%View{}, %{}, [])

  @doc "Returns the changeset representing the view with the given id."
  @spec changeset(integer) :: Changeset.t
  def changeset(view_id), do:
    Ecto.Changeset.cast(Repo.get!(View, view_id), %{}, [])

  @doc "Retrieves views of the given user for the given data source."
  @spec all(User.t, DataSource.t) :: [View.t]
  def all(user, data_source), do: by_user_id(user.id) |> by_data_source_id(data_source.id) |> Repo.all()

  @doc "Saves the new view in the database."
  @spec create(User.t, DataSource.t, String.t, String.t) :: {:ok, View.t} | {:error, Ecto.Changeset.t}
  def create(user, data_source, name, sql) do
    changes = %{data_source_id: data_source.id, user_id: user.id, name: name, sql: sql}
    with {:ok, changeset} <- validated_view_changeset(%View{}, user, changes, :insert), do:
      Repo.insert(changeset)
  end

  @doc "Updates the existing view in the database."
  @spec update(integer, User.t, String.t, String.t) :: {:ok, View.t} | {:error, Ecto.Changeset.t}
  def update(view_id, user, name, sql) do
    view = Repo.get!(View, view_id)

    # view must be owned by the user
    true = (view.user_id == user.id)

    changes = %{name: name, sql: sql}
    with {:ok, changeset} <- validated_view_changeset(view, user, changes, :update) do
      {:ok, view} = Repo.update(changeset)
      revalidate_views!(user, view.data_source_id)
      {:ok, view}
    end
  end

  @doc "Deletes the given view from the database."
  @spec delete(integer, User.t) :: :ok
  def delete(view_id, user) do
    {1, _} = Repo.delete_all(from view in View, where: view.id == ^view_id and view.user_id == ^user.id)
    :ok
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp revalidate_views!(user, data_source_id) do
    for view <- by_user_id(user.id) |> by_data_source_id(data_source_id) |> Repo.all() do
      case valid_view?(view, user) do
        {:ok, valid} -> view |> apply_view_changeset(%{broken: not valid}) |> Repo.update()
        :error -> :do_nothing
      end
    end
  end

  defp valid_view?(view, user) do
    case DataSource.validate_view({:id, view.data_source_id}, user, view) do
      {:ok, _} -> {:ok, true}
      {:error, "sql", _} -> {:ok, false}
      {:error, "name", _} -> {:ok, false}
      _ -> :error
    end
  end

  defp validated_view_changeset(view, user, changes, action) do
    changeset = %Ecto.Changeset{apply_view_changeset(view, changes) | action: action}

    if Enum.empty?(changeset.errors) do
      final_view = Ecto.Changeset.apply_changes(changeset)

      # make sure the user is correct
      true = (final_view.user_id == user.id)

      case DataSource.validate_view({:id, final_view.data_source_id}, user, final_view) do
        {:ok, columns} ->
          {:ok, Ecto.Changeset.put_change(changeset, :result_info, %{columns: columns})}
        {:error, "name", name_error} ->
          # Name error returned by the cloak -> we'll convert into a changeset
          {:error, Ecto.Changeset.add_error(changeset, :name, name_error)}
        {:error, "sql", sql_error} ->
          # SQL error returned by the cloak -> we'll convert into a changeset
          {:error, Ecto.Changeset.add_error(changeset, :sql, sql_error)}
        {:error, :not_connected} ->
          # Cloak not available
          {:error, Ecto.Changeset.add_error(changeset, :sql,
            "The view cannot be saved because no cloak is currently available for the given data source. " <>
            "Please contact your administrator."
          )}
        {:error, :expired} ->
          {:error, Ecto.Changeset.add_error(changeset, :sql,
            "Your Aircloak installation is running version #{Air.SharedView.version()} " <>
            "which expired on #{Version.expiry_date()}."
          )}
      end
    else
      {:error, changeset}
    end
  end

  defp apply_view_changeset(view, changes), do:
    view
    |> Ecto.Changeset.cast(changes, ~w(name sql user_id data_source_id broken)a)
    |> Ecto.Changeset.validate_required(~w(name sql user_id data_source_id)a)
    |> Ecto.Changeset.unique_constraint(:name, name: :views_user_id_data_source_id_name_index)

  defp by_user_id(scope \\ View, user_id), do: where(scope, [v], v.user_id == ^user_id)

  defp by_data_source_id(scope, data_source_id), do: where(scope, [v], v.data_source_id == ^data_source_id)
end

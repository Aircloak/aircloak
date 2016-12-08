defmodule Air.Service.View do
  @moduledoc "Service module for working with views."

  alias Air.Schemas.{User, View}
  alias Air.{Repo, Service.DataSource}
  import Ecto.Query, only: [from: 2]


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns the changeset representing an empty view."
  @spec new_changeset() :: View.t
  def new_changeset(), do:
    Ecto.Changeset.cast(%View{}, %{}, [])

  @doc "Returns the changeset representing the view with the given id."
  @spec changeset(integer) :: View.t
  def changeset(view_id), do:
    Ecto.Changeset.cast(Repo.get!(View, view_id), %{}, [])

  @doc "Retrieves views of the given user for the given data source."
  @spec all(User.t, DataSource.t) :: [View.t]
  def all(user, data_source), do:
    Repo.all(from view in View, where: view.data_source_id == ^data_source.id and view.user_id == ^user.id)

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
    with {:ok, changeset} <- validated_view_changeset(view, user, changes, :update), do:
      Repo.update(changeset)
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
            "Cannot validate the view SQL since no cloak is available for the given data source.")}
      end
    else
      {:error, changeset}
    end
  end

  defp apply_view_changeset(view, changes), do:
    view
    |> Ecto.Changeset.cast(changes, ~w(name sql user_id data_source_id)a)
    |> Ecto.Changeset.validate_required(~w(name sql user_id data_source_id)a)
    |> Ecto.Changeset.unique_constraint(:name, name: :views_user_id_data_source_id_name_index)
end

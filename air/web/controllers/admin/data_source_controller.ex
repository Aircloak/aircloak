defmodule Air.Admin.DataSourceController do
  @moduledoc """
  Controller for administrators to manage data sources.
  """

  use Air.Web, :admin_controller

  alias Air.{DataSource, DataSourceManager, AuditLog, User}


  # -------------------------------------------------------------------
  # Air.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{
      admin: :all
    }
  end


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    data_sources = Repo.all(DataSource) |> Repo.preload([:groups])
    data_sources = Enum.sort_by(data_sources, &{DataSourceManager.available?(&1.global_id), &1.name})

    query = from data_source in DataSource,
      inner_join: group in assoc(data_source, :groups),
      inner_join: user in assoc(group, :users),
      group_by: data_source.id,
      select: %{
        id: data_source.id,
        users_count: count(user.id, :distinct)
      }
    users_count = for data_source <- Repo.all(query), into: %{} do
      {data_source.id, data_source.users_count}
    end

    render(conn, "index.html", data_sources: data_sources, users_count: users_count)
  end

  def edit(conn, %{"id" => id}) do
    data_source = Repo.get(DataSource, id) |> Repo.preload([:groups])
    render(conn, "edit.html", changeset: DataSource.changeset(data_source), chosen_groups: data_source.groups)
  end

  def update(conn, %{"id" => id} = params) do
    user = Repo.get!(DataSource, id) |> Repo.preload([:groups])
    changeset = DataSource.changeset(user, params["data_source"])
    case Repo.update(changeset) do
      {:ok, data_source} ->
        AuditLog.log(conn, "Altered data_source", name: data_source.name)
        conn
        |> put_flash(:info, "Data source updated")
        |> redirect(to: admin_data_source_path(conn, :index))
      {:error, changeset} -> render(conn, "edit.html", changeset: changeset)
    end
  end

  def show(conn, %{"id" => id}) do
    data_source = Repo.get(DataSource, id) |> Repo.preload([:groups])

    query = from user in User,
      distinct: user.id,
      inner_join: group in assoc(user, :groups),
      inner_join: data_source in assoc(group, :data_sources),
      where: data_source.id == ^id,
      select: user
    users = Repo.all(query)

    render(conn, "show.html",
      data_source: data_source,
      conn: conn,
      users: users
    )
  end

  def delete(conn, %{"id" => id}) do
    data_source = Repo.get!(DataSource, id)
    Repo.delete!(data_source)
    AuditLog.log(conn, "Removed data source", name: data_source.name, global_id: data_source.global_id)
    conn
    |> put_flash(:info, "Data source deleted")
    |> redirect(to: "/admin/data_sources")
  end
end

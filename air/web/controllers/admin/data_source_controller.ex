defmodule Air.Admin.DataSourceController do
  @moduledoc """
  Controller for administrators to manage data sources.
  """

  use Air.Web, :admin_controller

  alias Air.{Schemas.DataSource, Schemas.User}

  plug :load_data_source when action in [:show, :edit, :update, :delete]


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
    data_sources = Enum.sort_by(data_sources,
      &{not Air.Service.DataSource.available?(&1.global_id), &1.name})

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

  def edit(conn, _params) do
    data_source = conn.assigns.data_source
    render(conn, "edit.html", changeset: DataSource.changeset(data_source), chosen_groups: data_source.groups)
  end

  def update(conn, params) do
    user = conn.assigns.data_source
    changeset = DataSource.changeset(user, params["data_source"])
    case Repo.update(changeset) do
      {:ok, data_source} ->
        audit_log(conn, "Altered data source", name: data_source.name, data_source: data_source.id)
        conn
        |> put_flash(:info, "Data source updated")
        |> redirect(to: admin_data_source_path(conn, :index))
      {:error, changeset} -> render(conn, "edit.html", changeset: changeset)
    end
  end

  def show(conn, _params) do
    data_source = conn.assigns.data_source

    query = from user in User,
      distinct: user.id,
      inner_join: group in assoc(user, :groups),
      inner_join: data_source in assoc(group, :data_sources),
      where: data_source.id == ^data_source.id,
      select: user,
      preload: [:groups]
    users = Repo.all(query)

    render(conn, "show.html",
      data_source: data_source,
      conn: conn,
      users: users
    )
  end

  def delete(conn, _params) do
    data_source = conn.assigns.data_source
    Repo.delete!(data_source)
    audit_log(conn, "Removed data source", name: data_source.name,
      global_id: data_source.global_id, data_source: data_source.id)
    conn
    |> put_flash(:info, "Data source deleted")
    |> redirect(to: admin_data_source_path(conn, :index))
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp load_data_source(conn, _) do
    case Repo.get(DataSource, conn.params["id"]) do
      nil ->
        conn
        |> put_layout(false)
        |> put_status(:not_found)
        |> render(Air.ErrorView, "404.html")
        |> halt()
      data_source ->
        data_source = Repo.preload(data_source, [:groups])
        assign(conn, :data_source, data_source)
    end
  end
end

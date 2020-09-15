defmodule AirWeb.Admin.CloaksController do
  @moduledoc false
  use Air.Web, :admin_controller
  use Timex

  alias Air.{Service.Cloak, Repo, Schemas.DataSource}

  plug(:load_cloak_info when action in [:reinitialize])

  # -------------------------------------------------------------------
  # AirWeb.VerifyPermissions callback
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
    cloak_groups =
      Cloak.all_cloak_infos()
      |> Enum.map(&load_data_sources/1)
      |> Enum.with_index()
      |> Enum.group_by(fn {_cloak, index} -> trunc(index / 2) end)
      |> Enum.map(fn {_group_index, cloaks} ->
        {cloaks, _} = Enum.unzip(cloaks)
        cloaks
      end)

    render(conn, "index.html", cloak_groups: cloak_groups, count: length(Cloak.all_cloak_infos()))
  end

  def reinitialize(conn, _params) do
    cloak_info = conn.assigns.cloak_info
    AirWeb.Socket.Cloak.MainChannel.reinitialize_all_data_sources(cloak_info.main_channel_pid)

    conn
    |> put_flash(:info, "Reinitializing data sources for #{cloak_info.name}. This may take up to a few minutes.")
    |> redirect(to: admin_cloaks_path(conn, :index))
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp load_cloak_info(conn, _) do
    case Cloak.cloak_info(conn.params["id"]) do
      nil -> not_found(conn)
      cloak_info -> assign(conn, :cloak_info, cloak_info)
    end
  end

  def load_data_sources(cloak_info) do
    data_sources = Enum.map(Map.keys(cloak_info.data_sources), &Repo.get_by(DataSource, name: &1))
    Map.merge(cloak_info, %{data_sources: data_sources})
  end
end

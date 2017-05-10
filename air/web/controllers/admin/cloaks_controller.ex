defmodule Air.Admin.CloaksController do
  @moduledoc false
  use Air.Web, :admin_controller
  use Timex

  alias Air.{Service.Cloak, Repo, Schemas.DataSource}


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
    cloak_groups = Cloak.all_cloak_infos()
    |> Enum.map(&load_data_sources/1)
    |> Enum.with_index()
    |> Enum.group_by(fn({_cloak, index}) -> trunc(index / 2) end)
    |> Enum.map(fn({_group_index, cloaks}) ->
      {cloaks, _} = Enum.unzip(cloaks)
      cloaks
    end)
    render(conn, "index.html", cloak_groups: cloak_groups, count: length(Cloak.all_cloak_infos()))
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  def load_data_sources(cloak_info) do
    data_sources = Enum.map(Map.keys(cloak_info.data_sources), &Repo.get_by(DataSource, name: &1))
    Map.merge(cloak_info, %{data_sources: data_sources})
  end
end

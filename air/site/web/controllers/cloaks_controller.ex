defmodule Air.CloaksController do
  @moduledoc false
  use Air.Web, :controller
  use Timex

  alias Air.{DataSourceManager, Repo, DataSource}


  # -------------------------------------------------------------------
  # Air.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{
      user: :all
    }
  end


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    cloaks = DataSourceManager.cloaks()
    |> Enum.map(&load_data_sources/1)
    render(conn, "index.html", cloaks: cloaks)
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  def load_data_sources(cloak_info) do
    data_sources = Enum.map(cloak_info.data_source_ids, &Repo.get_by(DataSource, unique_id: &1))
    Map.merge(cloak_info, %{data_sources: data_sources})
  end
end

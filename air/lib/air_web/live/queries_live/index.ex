defmodule AirWeb.QueriesLive.Index do
  @moduledoc false
  use Air.Web, :live_view

  @impl true
  def mount(_params, _session, socket) do
    {:ok, socket}
  end
end

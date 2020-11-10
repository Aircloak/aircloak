defmodule AirWeb.Admin.ExplorerLive.Index do
  @moduledoc false
  use Air.Web, :live_view

  alias Air.Service.{Explorer, Group}

  @impl true
  def mount(_params, session, socket) do
    if connected?(socket), do: AirWeb.Endpoint.subscribe("explorer")

    group = Explorer.group()

    {:ok,
     socket
     |> assign_new(:current_user, fn -> current_user!(session) end)
     |> assign(:changeset, Group.to_changeset(group))
     |> assign(all_data_sources: Explorer.all_data_source_metadata())}
  end

  @impl true
  def handle_info(%{event: "analysis_updated"}, socket) do
    {:noreply,
      socket
      |> assign(:all_data_sources, Explorer.all_data_source_metadata())
    }
  end

  @impl true
  def handle_event("reanalyze_datasource", %{"data_source_id" => id}, socket) do
    data_source_id = String.to_integer(id)
    socket.assigns.all_data_sources
    |> Enum.filter(& &1.id == data_source_id)
    |> Enum.flat_map(& &1.selected_tables)
    |> Enum.each(& Explorer.analyze_table(data_source_id, &1))

    {:noreply, socket}
  end

  def handle_event("toggle_table", %{"_target" => [data_source_id_str, table_name]} = payload, socket) do
    data_source_id = String.to_integer(data_source_id_str)
    state = payload[data_source_id_str][table_name]
    cond do
      state == "on" -> Explorer.analyze_table(data_source_id, table_name)
      is_nil state -> Explorer.disable_table(data_source_id, table_name)
    end
    {:noreply, socket}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp current_user!(%{"_air_session_token" => token}) do
    {:ok, user_id} = Air.Service.RevokableToken.verify(token, :session)
    {:ok, user} = Air.Service.User.load_enabled(user_id)
    user
  end

  defp analyzable_data_sources(data_sources), do:
    Enum.reject(data_sources, & &1.tables == [])
end

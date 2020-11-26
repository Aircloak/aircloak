defmodule AirWeb.Admin.ExplorerLive.Index do
  @moduledoc false
  use Air.Web, :live_view

  alias Air.Service.{AuditLog, Explorer, Group}

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
     |> assign(:all_data_sources, Explorer.all_data_source_metadata())}
  end

  @impl true
  def handle_event("reanalyze_datasource", %{"data_source_id" => id}, socket) do
    data_source_id = String.to_integer(id)

    socket.assigns.all_data_sources
    |> Enum.filter(&(&1.id == data_source_id))
    |> Enum.flat_map(& &1.selected_tables)
    |> Enum.each(&Explorer.analyze_table(data_source_id, &1))

    {:noreply, socket}
  end

  def handle_event("analyze_table", %{"data_source_id" => data_source_id_str, "table_name" => table_name}, socket) do
    data_source_id = String.to_integer(data_source_id_str)
    data_source = Enum.find(socket.assigns.all_data_sources, &(&1.id == data_source_id))

    AuditLog.log(
      socket.assigns.current_user,
      "Added table to Diffix Explorer",
      %{
        data_source: data_source.name,
        table: table_name
      }
    )

    Explorer.analyze_table(data_source_id, table_name)
    {:noreply, socket}
  end

  def handle_event("disable_table", %{"data_source_id" => data_source_id_str, "table_name" => table_name}, socket) do
    data_source_id = String.to_integer(data_source_id_str)
    data_source = Enum.find(socket.assigns.all_data_sources, &(&1.id == data_source_id))

    AuditLog.log(
      socket.assigns.current_user,
      "Removed table from Diffix Explorer",
      %{
        data_source: data_source.name,
        table: table_name
      }
    )

    Explorer.disable_table(data_source_id, table_name)
    {:noreply, socket}
  end
end

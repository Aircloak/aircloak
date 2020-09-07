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
     |> assign(:data_sources, Explorer.statistics())
     |> assign(:changeset, Group.to_changeset(group))
     |> assign(all_data_sources: Enum.map(Air.Service.DataSource.all(), &{{&1, selected_tables(&1)}, &1.id}))}
  end

  @impl true
  def handle_info(%{event: "analysis_updated"}, socket) do
    stats = Explorer.statistics()
    {:noreply, socket |> assign(:data_sources, stats)}
  end

  @impl true
  def handle_event("reanalyze_datasource", %{"data_source_id" => id}, socket) do
    {:ok, data_source} =
      Air.Service.DataSource.fetch_as_user(
        {:name, id},
        Explorer.user()
      )

    Explorer.reanalyze_datasource(data_source)
    {:noreply, socket |> assign(:data_sources, Explorer.statistics())}
  end

  def handle_event("save_settings", params, socket) do
    before = Explorer.group()

    {:noreply,
     case Explorer.change_permitted_data_sources(params["group"]) do
       {:ok, group} ->
         audit_log(socket, "Altered Diffix Explorer permissions", before: before, after: group)

         socket
         |> assign(:data_sources, Explorer.statistics())
         |> assign(:changeset, Group.to_changeset(group))
         |> assign(all_data_sources: Enum.map(Air.Service.DataSource.all(), &{{&1, selected_tables(&1)}, &1.id}))
         |> put_flash(:info, "Diffix Explorer settings updated. It can take some time before you see new results.")

       {:error, changeset} ->
         socket
         |> assign(:changeset, changeset)
     end}
  end

  def handle_event("reanalyze_all", _params, socket) do
    Air.Service.DataSource.all()
    |> Enum.filter(&Explorer.data_source_enabled?/1)
    |> Enum.each(&Explorer.reanalyze_datasource/1)

    {:noreply,
     socket
     |> assign(:data_sources, Explorer.statistics())
     |> put_flash(:info, "Reanalyzing all data sources.")}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp selected_tables(data_source) do
    if Explorer.data_source_enabled?(data_source) do
      Enum.map(Explorer.results_for_datasource(data_source), & &1.table_name)
    else
      Explorer.elligible_tables_for_datasource(data_source)
    end
  end

  defp current_user!(%{"_air_session_token" => token}) do
    {:ok, user_id} = Air.Service.RevokableToken.verify(token, :session)
    {:ok, user} = Air.Service.User.load_enabled(user_id)
    user
  end

  defp checkbox_mapper(form, field, input_opts, {data_source, selected_tables}, label_opts, _opts) do
    tables =
      Explorer.elligible_tables_for_datasource(data_source)
      |> Enum.map(&{&1, &1})

    checked = Keyword.get(input_opts, :checked, false)

    content_tag(:div, class: "form-check") do
      [
        tag(:input, [
          {:class, "form-check-input"},
          {:"data-target", "#tables-#{data_source.id}"},
          {:"data-toggle", "collapse"} | input_opts
        ]),
        label(form, field, [{:class, "form-check-label"} | label_opts]) do
          data_source.name
        end,
        content_tag(:div, class: "collapse #{if checked, do: "show"} nested-checkboxes", id: "tables-#{data_source.id}") do
          content_tag(:div) do
            PhoenixMTM.Helpers.collection_checkboxes(form, :tables, tables,
              selected: selected_tables,
              mapper: &table_checkbox_mapper/6,
              data_source_id: data_source.id
            )
          end
        end
      ]
    end
  end

  defp table_checkbox_mapper(form, field, input_opts, table_name, label_opts, opts) do
    data_source_id = Keyword.get(opts, :data_source_id)

    input_opts =
      input_opts
      |> Keyword.update!(:name, fn name -> name <> "[#{data_source_id}]" end)
      |> Keyword.put(:class, "form-check-input")

    content_tag(:div, class: "form-check") do
      [
        tag(:input, input_opts),
        label(form, field, [{:class, "form-check-label text-break mw-100"} | label_opts]) do
          table_name
        end
      ]
    end
  end
end

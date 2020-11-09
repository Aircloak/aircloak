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
    with {:ok, data_source} <-
      Air.Service.DataSource.fetch_as_user(
        {:name, id},
        Explorer.user()
      ) do
      Explorer.reanalyze_datasource(data_source)
    end

    {:norelpy, assign(socket, :all_data_sources, Explorer.all_data_source_metadata())}
  end

  def handle_event("save_settings", params, socket) do
    before = Explorer.group()

    {:noreply,
     case Explorer.change_permitted_data_sources(params["group"]) do
       {:ok, group} ->
         audit_log(socket, "Altered Diffix Explorer permissions", before: before, after: group)

         socket
         |> assign(:changeset, Group.to_changeset(group))
         |> assign(all_data_sources: Explorer.all_data_source_metadata())
         |> put_flash(:info, "Diffix Explorer settings updated. It can take some time before you see new results.")

       {:error, changeset} ->
         assign(socket, :changeset, changeset)
     end}
  end

  def handle_event("reanalyze_all", _params, socket) do
    Air.Service.DataSource.all()
    |> Enum.filter(&Explorer.data_source_enabled?/1)
    |> Enum.each(&Explorer.reanalyze_datasource/1)

    {:noreply,
     socket
     |> assign(all_data_sources: Explorer.all_data_source_metadata())
     |> put_flash(:info, "Reanalyzing all data sources.")}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp current_user!(%{"_air_session_token" => token}) do
    {:ok, user_id} = Air.Service.RevokableToken.verify(token, :session)
    {:ok, user} = Air.Service.User.load_enabled(user_id)
    user
  end

  defp enabled_data_sources(data_sources), do:
    Enum.filter(data_sources, & length(&1.selected_tables) > 0)

  defp checkbox_mapper(form, field, input_opts, data_source, label_opts, _opts) do
    checked = Keyword.get(input_opts, :checked, false)

    marked_as_selected =
      if data_source[:selected_tables] == [] do
        data_source[:eligible_tables]
      else
        data_source[:selected_tables]
      end

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
            PhoenixMTM.Helpers.collection_checkboxes(form, :tables, Enum.map(data_source.eligible_tables, & {&1, &1}),
              selected: marked_as_selected,
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

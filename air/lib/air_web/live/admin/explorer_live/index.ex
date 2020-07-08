defmodule AirWeb.Admin.ExplorerLive.Index do
  use Air.Web, :live_view

  alias Air.Service.{Explorer, Group}

  @impl true
  def mount(params, session, socket) do
    if connected?(socket), do: AirWeb.Endpoint.subscribe("explorer")

    group = Explorer.group()

    {:ok,
     socket
     |> assign_new(:current_user, fn -> current_user!(session) end)
     |> assign(:data_sources, Explorer.statistics())
     |> assign(:changeset, Group.to_changeset(group))
     |> assign(all_data_sources: Enum.map(Air.Service.DataSource.all(), &{{&1.name, &1.description}, &1.id}))}
  end

  @impl true
  def handle_info(%{event: "analysis_updated"}, socket) do
    stats = Explorer.statistics()
    {:noreply, socket |> assign(:data_sources, stats)}
  end

  defp current_user!(%{"_air_session_token" => token}) do
    {:ok, user_id} = Air.Service.RevokableToken.verify(token, :session)
    {:ok, user} = Air.Service.User.load_enabled(user_id)
    user
  end

  defp checkbox_mapper(form, field, input_opts, {name, _description}, label_opts, _opts) do
    content_tag(:div, class: "form-check") do
      [
        tag(:input, [{:class, "form-check-input"} | input_opts]),
        label(form, field, [{:class, "form-check-label"} | label_opts]) do
          name
        end
      ]
    end
  end
end

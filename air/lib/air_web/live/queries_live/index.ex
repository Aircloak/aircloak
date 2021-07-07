defmodule AirWeb.QueriesLive.Index do
  @moduledoc false
  use Air.Web, :live_view

  alias Air.Service.DataSource
  alias Air.Service.Query
  alias Plug.CSRFProtection

  @page_size 10

  @impl true
  def mount(_params, session, socket) do
    {
      :ok,
      socket
      |> assign(:max_results, @page_size)
      |> assign(:query_states, [
        %{value: "running", label: "Running"},
        %{value: "completed", label: "Completed"},
        %{value: "failed", label: "Failed"},
        %{value: "cancelled", label: "Cancelled"}
      ])
      |> assign_new(:current_user, fn -> current_user!(session) end)
      |> then_assign_new(:data_sources, fn socket ->
        DataSource.for_user(socket.assigns.current_user) |> Enum.map(&%{label: &1.name, value: &1.id})
      end)
      |> then_assign(fn socket ->
        [
          csrf_token: CSRFProtection.get_csrf_token(),
          number_format: Air.Service.User.number_format_settings(socket.assigns.current_user),
          debug_mode_enabled: socket.assigns.current_user.debug_mode_enabled
        ]
      end)
    }
  end

  @impl true
  def handle_params(params, _uri, socket) do
    from = parse_datetime(params["from"], Timex.now() |> Timex.shift(months: -1))
    to = parse_datetime(params["to"], Timex.now())
    phrase = get_phrase(params)

    {
      :noreply,
      socket
      |> assign(:query_params, params)
      |> assign(:from, from)
      |> assign(:to, to)
      |> assign(:phrase, phrase)
      |> if_connected(fn socket ->
        filters = %{
          from: from,
          to: to,
          phrase: phrase,
          query_states: get_query_states(params["query_states"]),
          data_sources: params["data_sources"] || [],
          users: [socket.assigns.current_user.id],
          max_results: @page_size
        }

        queries =
          Query.queries(filters)
          |> Enum.map(fn query ->
            AirWeb.Query.for_display(query,
              authenticated?: true,
              buckets: Query.buckets(query, 0)
            )
          end)

        push_event(socket, "queries", %{queries: queries})
      end)
    }
  end

  @impl true
  def handle_event("change_dates", params, socket) do
    {
      :noreply,
      push_patch(socket,
        to:
          queries_index_path(
            socket,
            :index,
            Map.merge(
              socket.assigns.query_params,
              %{from: params["from"], to: params["to"]}
            )
          )
      )
    }
  end

  @impl true
  def handle_event("change_phrase", params, socket) do
    {
      :noreply,
      push_patch(socket,
        to:
          queries_index_path(
            socket,
            :index,
            Map.put(
              socket.assigns.query_params,
              :phrase,
              get_phrase(params)
            )
          )
      )
    }
  end

  # -------------------------------------------------------------------
  # Helpers
  # -------------------------------------------------------------------

  defp parse_datetime(value, default) do
    case Timex.parse(value, "{ISOdate} {ISOtime}") do
      {:ok, result} ->
        result

      _error ->
        default
    end
  end

  defp get_phrase(params) do
    String.trim(params["phrase"] || "")
    |> case do
      "" -> nil
      phrase -> phrase
    end
  end

  defp get_query_states(nil), do: []
  defp get_query_states(states), do: Enum.flat_map(states, &parse_query_state/1)

  defp parse_query_state("running"), do: Air.Service.Query.State.active()
  defp parse_query_state("completed"), do: [:completed]
  defp parse_query_state("failed"), do: [:error]
  defp parse_query_state("cancelled"), do: [:cancelled]
  defp parse_query_state(_), do: []

  defp if_connected(socket, func) do
    if connected?(socket), do: func.(socket), else: socket
  end

  defp then_assign(socket, func), do: assign(socket, func.(socket))

  defp then_assign_new(socket, key, func), do: assign_new(socket, key, fn -> func.(socket) end)
end

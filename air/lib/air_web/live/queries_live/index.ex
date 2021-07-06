defmodule AirWeb.QueriesLive.Index do
  @moduledoc false
  use Air.Web, :live_view

  alias Air.Service.DataSource
  alias Air.Service.Query

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
        DataSource.for_user(socket.assigns.current_user) |> Enum.map(&%{label: &1.name, value: &1.name})
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
      |> assign(:queries, [])
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

  defp then_assign_new(socket, key, func), do: assign_new(socket, key, fn -> func.(socket) end)
end

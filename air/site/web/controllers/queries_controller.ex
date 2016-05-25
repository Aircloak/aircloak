defmodule Air.QueriesController do
  @moduledoc false
  use Air.Web, :controller
  use Timex

  require Logger
  alias Air.{Task, Repo}

  # -------------------------------------------------------------------
  # Air.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{user: :all}
  end


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    render(conn, "index.html",
      guardian_token: Guardian.Plug.current_token(conn),
      csrf_token: Plug.CSRFProtection.get_csrf_token(),
      recent_results: Poison.encode!(recent_tasks(conn.assigns.current_user)),
      data_sources: Poison.encode!(data_sources(conn))
    )
  end

  def run_query(conn, %{"query" => params}) do
    {:ok, task} = build_assoc(conn.assigns.current_user, :tasks)
    |> Task.changeset(parse_task_params(params))
    |> Repo.insert()

    try do
      case Air.Socket.Cloak.MainChannel.run_task(task.cloak_id, Task.to_cloak_query(task)) do
        :ok ->
          json(conn, %{success: true})
        {:error, :not_connected} ->
          json(conn, %{success: false, reason: "the cloak is not connected"})
        {:error, reason} ->
          Logger.error(fn -> "Task start error: #{reason}" end)
          json(conn, %{success: false, reason: reason})
      end
    catch type, error ->
      # We'll make a nice error log report and return 500
      Logger.error([
        "Error starting a task: #{inspect(type)}:#{inspect(error)}\n",
        Exception.format_stacktrace(System.stacktrace())
      ])

      send_resp(conn, Plug.Conn.Status.code(:internal_server_error), "")
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp recent_tasks(user) do
    user
    |> Task.for_user
    |> Task.recent(_recent_count = 5)
    |> Repo.all
    |> Enum.map(&encode_task/1)
  end

  defp encode_task(task) do
    %{query: task.query}
  end

  defp data_sources(conn) do
      for cloak <- Air.CloakInfo.all(conn.assigns.current_user.organisation),
          data_source <- cloak.data_sources
      do
        %{
          id: data_source.id,
          display: "#{data_source.id} (#{cloak.name})",
          tables: data_source.tables,
          cloak: cloak,
          token: data_source_token(cloak.id, data_source.id)
        }
      end
  end

  defp parse_task_params(params) do
    {cloak_id, data_source} = decode_data_source_token(params["data_source_token"])
    Map.merge(params, %{"cloak_id" => cloak_id, "data_source" => data_source})
  end

  defp data_source_token(nil, nil), do: nil
  defp data_source_token(cloak_id, data_source) do
    Phoenix.Token.sign(Air.Endpoint, "data_source_token", {cloak_id, data_source})
  end

  defp decode_data_source_token(nil), do: {nil, nil}
  defp decode_data_source_token(data_source_token) do
    {:ok, {cloak_id, data_source}} = Phoenix.Token.verify(Air.Endpoint, "data_source_token", data_source_token)
    {cloak_id, data_source}
  end
end

defmodule Compliance.DataSource.Drill do
  @moduledoc false

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  use Compliance.DataSource.Connector
  alias Compliance.DataSource.Connector
  alias Cloak.DataSource.Drill

  @impl Connector
  def setup(%{parameters: params}) do
    Application.ensure_all_started(:httpoison)
    Application.ensure_all_started(:odbc)

    port = Map.get(params, :management_port, 8047)
    Connector.await_port(params.hostname, port)
    configure_storage_plugin(params.hostname, port, params.workspace, params.drill_data_dir)

    :ok
  end

  @impl Connector
  def connect(state), do: Map.put(state, :conn, Drill.connect!(state.parameters))

  @impl Connector
  def create_table(_table_name, _columns, state), do: state

  @impl Connector
  def after_tables_created(state), do: state

  @impl Connector
  def insert_rows(table_name, data, state) do
    jsonized =
      data
      |> Enum.map(&format_datetimes/1)
      |> Enum.map(&Poison.encode!/1)
      |> Enum.join("\n")

    state.parameters.data_dir
    |> Path.join("#{table_name}_raw")
    |> File.write!(jsonized)

    create_view(table_name, data, state.conn)

    state
  end

  @impl Connector
  def terminate(state), do: :odbc.disconnect(state.conn)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp format_datetimes(data) do
    update_in(data, [Lens.map_values() |> Lens.filter(&match?(%NaiveDateTime{}, &1))], &to_string/1)
  end

  defp create_view(table_name, [item | _data], conn) do
    run_query("DROP VIEW IF EXISTS #{table_name}", conn)
    run_query("CREATE VIEW #{table_name} AS SELECT #{view_select_list(item)} FROM #{table_name}_raw", conn)
  end

  defp view_select_list(item) do
    item
    |> Enum.map(fn
      {field, %Date{}} -> "CAST(`#{field}` AS date) AS `#{field}`"
      {field, %Time{}} -> "CAST(`#{field}` AS time) AS `#{field}`"
      {field, %NaiveDateTime{}} -> "CAST(`#{field}` AS timestamp) AS `#{field}`"
      {field, _} -> "`#{field}`"
    end)
    |> Enum.join(", ")
  end

  defp run_query(query, conn) do
    case :odbc.sql_query(conn, to_charlist(query)) do
      {:error, reason} -> raise to_string(reason)
      _ -> :ok
    end
  end

  defp configure_storage_plugin(drill_hostname, drill_port, workspace, drill_data_dir) do
    config =
      Poison.encode!(%{
        type: "file",
        enabled: true,
        connection: "file:///",
        config: nil,
        workspaces: %{
          workspace => %{
            location: drill_data_dir,
            writable: true,
            defaultInputFormat: "json",
            allowAccessOutsideWorkspace: false
          }
        },
        formats: %{
          json: %{
            type: "json",
            extensions: ["json", ""]
          }
        }
      })

    data = "name=dfs&config=#{URI.encode_www_form(config)}"

    %{status_code: 200} =
      HTTPoison.post!("http://#{drill_hostname}:#{drill_port}/storage/dfs", data, [
        {"Content-Type", "application/x-www-form-urlencoded"}
      ])
  end
end

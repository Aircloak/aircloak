defmodule Compliance.DataSource.Drill do
  @moduledoc false

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  use Compliance.DataSource.Connector
  alias Compliance.DataSource.Connector

  @impl Connector
  def setup(options = %{parameters: params}) do
    Application.ensure_all_started(:httpoison)

    port = Map.get(params, :management_port, 8047)
    Connector.await_port(params.hostname, port)
    configure_storage_plugin(params.hostname, port, params.workspace, params.drill_data_dir)

    params
  end

  @impl Connector
  def connect(state), do: state

  @impl Connector
  def create_table(table_name, columns, state), do: state

  @impl Connector
  def after_tables_created(state), do: state

  @impl Connector
  def insert_rows(table_name, data, state) do
    jsonized =
      data
      |> Enum.map(&Poison.encode!/1)
      |> Enum.join("\n")

    state.parameters.data_dir
    |> Path.join(table_name)
    |> File.write!(jsonized)

    state
  end

  @impl Connector
  def terminate(state), do: :ok

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

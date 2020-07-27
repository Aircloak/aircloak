defmodule Air.Service.ExplorerTest do
  # because of shared mode

  use Air.SchemaCase, async: false

  alias Air.Service.{DataSource, Explorer, Group}
  alias Air.Schemas.ExplorerAnalysis
  require Aircloak.DeployConfig
  import Aircloak.AssertionHelper

  @moduletag capture_log: true

  defmodule MockServer do
    @moduledoc "Mocks the [Diffix Explorer API](https://github.com/diffix/explorer#usage)."

    use Agent

    @initial_state %{
      blocked: false,
      responses: %{
        foos:
          {:ok,
           %{
             status: "Complete",
             columns: [
               %{column: "user_id", metrics: []},
               %{column: "foo", metrics: [%{value: [32], key: "some-metric"}]}
             ],
             sampleData: []
           }},
        bars: {:error}
      }
    }

    def start_link(_) do
      Agent.start_link(fn -> @initial_state end, name: __MODULE__)
    end

    def reset() do
      Agent.update(__MODULE__, fn _ -> @initial_state end)
    end

    # Pausing

    def sleep_while_paused() do
      if Agent.get(__MODULE__, & &1.blocked) do
        Process.sleep(100)
        sleep_while_paused()
      end
    end

    def pause do
      Agent.update(__MODULE__, fn state -> %{state | blocked: true} end)
    end

    def resume do
      Agent.update(__MODULE__, fn state -> %{state | blocked: false} end)
    end

    # Response control

    def set_reponse(table, resp) do
      Agent.update(__MODULE__, &put_in(&1, [:responses, table], resp))
    end

    def get_response(table) do
      Agent.get(__MODULE__, &get_in(&1, [:responses, String.to_atom(table)]))
    end

    defmodule Controller do
      use Phoenix.Controller, namespace: AirWeb

      def explore(conn, params) do
        json(conn, %{
          status: "New",
          columns: [],
          sampleData: [],
          id: params["Table"],
          versionInfo: %{
            commitHash: "7a35d2c8cd661947a6916179b49e6381f4878268",
            commitRef: "master"
          }
        })
      end

      def result(conn, %{"id" => id}) do
        MockServer.sleep_while_paused()

        case MockServer.get_response(id) do
          {:error} ->
            resp(conn, 500, "Something went wrong")

          {:ok, data} ->
            json(
              conn,
              Map.merge(data, %{
                id: id,
                versionInfo: %{
                  commitHash: "7a35d2c8cd661947a6916179b49e6381f4878268",
                  commitRef: "master"
                }
              })
            )
        end
      end
    end

    defmodule Router do
      use Air.Web, :router

      pipeline :my_api do
        plug(:accepts, ["json"])
      end

      scope "/explorer" do
        pipe_through([:my_api])
        post("/explore", Controller, :explore)
        get("/result/:id", Controller, :result)
      end
    end

    defmodule Endpoint do
      use Phoenix.Endpoint, otp_app: :air

      plug(
        Plug.Parsers,
        parsers: [:urlencoded, :multipart, :json],
        pass: ["*/*"],
        json_decoder: Jason
      )

      plug(Router)

      def init(:supervisor, config) do
        res =
          Keyword.merge(config,
            server: true,
            secret_key_base: "fw34f43",
            http: [port: 3289],
            debug_errors: false,
            url: [port: 3289, scheme: "http", host: "localhost"]
          )

        {:ok, res}
      end
    end
  end

  @foos_table %{
    columns: [
      %{
        name: "user_id",
        user_id: true,
        isolated: true
      },
      %{
        name: "foo",
        user_id: false,
        isolated: false
      }
    ],
    id: "foos"
  }

  @bars_table %{
    columns: [
      %{
        name: "user_id",
        user_id: true,
        isolated: true
      },
      %{
        name: "bar",
        user_id: false,
        isolated: false
      }
    ],
    id: "bars"
  }

  setup do
    config = Application.get_env(:air, Aircloak.DeployConfig)
    start_supervised!(MockServer)
    start_supervised!(MockServer.Endpoint)

    MockServer.reset()

    Application.put_env(
      :air,
      Aircloak.DeployConfig,
      Map.put(config, "explorer", %{"url" => MockServer.Endpoint.url() <> "/explorer"})
    )

    on_exit(fn -> Application.put_env(:air, Aircloak.DeployConfig, config) end)

    start_supervised!(Explorer)

    tables = [@foos_table, @bars_table]

    ds1 = Air.TestRepoHelper.create_data_source!(%{tables: Jason.encode!(tables)})
    Group.update!(Explorer.group(), %{data_sources: [ds1.id]})

    ds_not_included = Air.TestRepoHelper.create_data_source!(%{tables: Jason.encode!(tables)})

    Explorer.change_permitted_data_sources(%{
      "data_sources" => [ds1.id],
      "tables" => [
        %{"#{ds1.id}" => "bars"},
        %{"#{ds1.id}" => "foos"}
      ]
    })

    %{ds1: ds1, ds_not_included: ds_not_included}
  end

  describe ".enabled?" do
    test "returns true when config exists" do
      assert Explorer.enabled?()
    end

    test "returns false when config doesn't exist" do
      config = Application.get_env(:air, Aircloak.DeployConfig)
      Application.put_env(:air, Aircloak.DeployConfig, Map.delete(config, "explorer"))
      refute Explorer.enabled?()
    end
  end

  describe "statistics" do
    test "returns statistics in various stages", %{ds1: ds1} do
      MockServer.pause()
      Explorer.reanalyze_datasource(ds1)

      data_source_id = ds1.id

      assert_soon(
        [
          %{
            complete: 0,
            processing: 2,
            error: 0,
            total: 2,
            id: ^data_source_id
          }
        ] = Explorer.statistics()
      )

      MockServer.resume()

      assert_soon(
        [
          %{
            complete: 1,
            processing: 0,
            error: 1,
            total: 2,
            id: ^data_source_id
          }
        ] = Explorer.statistics()
      )
    end
  end

  describe ".data_source_enabled?" do
    test "returns true if source included in config", context do
      assert Explorer.data_source_enabled?(context.ds1)
    end

    test "returns false if source not included in config", context do
      assert not Explorer.data_source_enabled?(context.ds_not_included)
    end
  end

  describe ".setup_credentials_if_required" do
    test "creates a user and group" do
      {:ok, user} = Air.Service.User.get_by_login("diffix-explorer@aircloak.com")
      Air.Service.User.delete!(user)
      {:ok, group} = Air.Service.Group.get_by_name("Diffix Explorer")
      Air.Service.Group.delete!(group)
      assert :ok = Explorer.setup_credentials_if_required()
      assert {:ok, _} = Air.Service.User.get_by_login("diffix-explorer@aircloak.com")
      assert {:ok, _} = Air.Service.Group.get_by_name("Diffix Explorer")
    end

    test "is indempotent" do
      assert :ok = Explorer.setup_credentials_if_required()
      users = Air.Service.User.all()
      assert :ok = Explorer.setup_credentials_if_required()
      assert users == Air.Service.User.all()
    end
  end

  describe ".data_source_updated" do
    test "removes results if table no longer exists in the data source", context do
      assert [
               %ExplorerAnalysis{table_name: "bars"},
               %ExplorerAnalysis{table_name: "foos"}
             ] = Enum.sort_by(Explorer.results_for_datasource(context.ds1), & &1.table_name)

      DataSource.update!(context.ds1, %{
        tables: Jason.encode!([@bars_table])
      })

      assert [
               %ExplorerAnalysis{table_name: "bars"}
             ] = Explorer.results_for_datasource(context.ds1)
    end
  end

  describe ".reanalyze_datasource" do
    test "creates jobs and polls for results", context do
      MockServer.pause()
      assert :ok == Explorer.reanalyze_datasource(context.ds1)

      assert_soon(
        [
          %ExplorerAnalysis{table_name: "bars", status: :new},
          %ExplorerAnalysis{table_name: "foos", status: :new}
        ] = Enum.sort_by(Explorer.results_for_datasource(context.ds1), & &1.table_name),
        timeout: 500
      )

      MockServer.resume()

      # Here we wait for polling to happen
      assert_soon(
        [
          %ExplorerAnalysis{
            table_name: "bars",
            status: :error,
            results: %{"columns" => [], "sampleData" => []},
            errors: ["Something went wrong in Diffix Explorer (HTTP 500) when polling: Something went wrong"]
          },
          %ExplorerAnalysis{
            table_name: "foos",
            status: :complete,
            results: %{
              "columns" => [
                %{"column" => "user_id", "metrics" => []},
                %{"column" => "foo", "metrics" => [%{"key" => "some-metric", "value" => [32]}]}
              ],
              "sampleData" => []
            }
          }
        ] = Enum.sort_by(Explorer.results_for_datasource(context.ds1), & &1.table_name),
        timeout: 500
      )
    end
  end

  describe "change_permitted_data_sources" do
    test "it removes all data sources that currently do not have permissions", context do
      id_ds1 = "#{context.ds1.id}"
      id_ds_not_included = "#{context.ds_not_included.id}"

      assert {:ok, _} =
               Explorer.change_permitted_data_sources(%{
                 "data_sources" => [context.ds1.id, context.ds_not_included.id],
                 "tables" => [
                   %{id_ds1 => "bars"},
                   %{id_ds1 => "foos"},
                   %{id_ds_not_included => "bars"},
                   %{id_ds_not_included => "foos"}
                 ]
               })

      assert_soon not Enum.empty?(Explorer.results_for_datasource(context.ds_not_included))

      assert {:ok, _} =
               Explorer.change_permitted_data_sources(%{
                 "data_sources" => [context.ds1.id],
                 "tables" => [
                   %{id_ds1 => "bars"},
                   %{id_ds1 => "foos"},
                   %{id_ds_not_included => "bars"},
                   %{id_ds_not_included => "foos"}
                 ]
               })

      assert_soon Enum.empty?(Explorer.results_for_datasource(context.ds_not_included))
    end

    test "it adds results for newly authorized data sources", context do
      id_ds1 = "#{context.ds1.id}"
      id_ds_not_included = "#{context.ds_not_included.id}"

      assert {:ok, _} =
               Explorer.change_permitted_data_sources(%{
                 "data_sources" => [context.ds1.id, context.ds_not_included.id],
                 "tables" => [
                   %{id_ds1 => "bars"},
                   %{id_ds1 => "foos"},
                   %{id_ds_not_included => "bars"},
                   %{id_ds_not_included => "foos"}
                 ]
               })

      assert_soon(
        [
          %ExplorerAnalysis{
            table_name: "bars",
            status: :error
          },
          %ExplorerAnalysis{
            table_name: "foos",
            status: :complete,
            results: %{
              "columns" => [
                %{"column" => "user_id", "metrics" => []},
                %{"column" => "foo", "metrics" => [%{"key" => "some-metric", "value" => [32]}]}
              ],
              "sampleData" => []
            }
          }
        ] = Enum.sort_by(Explorer.results_for_datasource(context.ds_not_included), & &1.table_name),
        timeout: 500
      )
    end

    test "it does not attempt to get results for already existing data sources", context do
      # Setup: make sure we have ds1 one ready and results already fetched
      id_ds1 = "#{context.ds1.id}"
      id_ds_not_included = "#{context.ds_not_included.id}"

      assert {:ok, _} =
               Explorer.change_permitted_data_sources(%{
                 "data_sources" => [context.ds1.id],
                 "tables" => [
                   %{id_ds1 => "bars"},
                   %{id_ds1 => "foos"}
                 ]
               })

      assert_soon(
        [
          %ExplorerAnalysis{
            table_name: "bars",
            status: :error
          },
          %ExplorerAnalysis{}
        ] = Enum.sort_by(Explorer.results_for_datasource(context.ds1), & &1.table_name),
        timeout: 500
      )

      # Now change what the server will have to say about bar
      MockServer.set_reponse(
        :bars,
        {:ok,
         %{
           status: "Processing",
           results: %{
             "columns" => [
               %{"column" => "foo", "metrics" => [%{"key" => "some-other-metric", "value" => [10]}]}
             ],
             "sampleData" => []
           }
         }}
      )

      assert {:ok, _} =
               Explorer.change_permitted_data_sources(%{
                 "data_sources" => [context.ds1.id, context.ds_not_included.id],
                 "tables" => [
                   %{id_ds1 => "bars"},
                   %{id_ds1 => "foos"},
                   %{id_ds_not_included => "bars"},
                   %{id_ds_not_included => "foos"}
                 ]
               })

      # Here we wait for ds_not_included to return from polling
      assert_soon(
        [
          %ExplorerAnalysis{
            table_name: "bars",
            status: :processing
          },
          %ExplorerAnalysis{}
        ] = Enum.sort_by(Explorer.results_for_datasource(context.ds_not_included), & &1.table_name),
        timeout: 500
      )

      # The point of this test: notice that status is still error, which shows that this data source was not refreshed
      assert_soon(
        [
          %ExplorerAnalysis{
            table_name: "bars",
            status: :error
          },
          %ExplorerAnalysis{}
        ] = Enum.sort_by(Explorer.results_for_datasource(context.ds1), & &1.table_name),
        timeout: 500
      )
    end
  end
end

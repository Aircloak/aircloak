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

      scope "/explorer/api/v1" do
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
            url: [port: 3289, scheme: "http", host: "localhost"],
            drainer: [
              shutdown: 100
            ]
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
        isolated: true,
        access: "visible"
      },
      %{
        name: "foo",
        user_id: false,
        isolated: false,
        access: "visible"
      }
    ],
    id: "foos"
  }

  @bars_table %{
    columns: [
      %{
        name: "user_id",
        user_id: true,
        isolated: true,
        access: "visible"
      },
      %{
        name: "bar",
        user_id: false,
        isolated: false,
        access: "visible"
      }
    ],
    id: "bars"
  }

  @bazs_table %{
    columns: [
      %{
        name: "user_id",
        user_id: true,
        isolated: true,
        access: "visible"
      },
      %{
        name: "unselectable",
        user_id: false,
        isolated: false,
        access: "unselectable"
      }
    ],
    id: "bazs"
  }

  setup_all do
    start_supervised!(MockServer.Endpoint)
    :ok
  end

  setup do
    config = Application.get_env(:air, Aircloak.DeployConfig)
    start_supervised!(MockServer)

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

  describe ".all_data_source_metadata" do
    test "returns statistics in various stages", %{ds1: ds1} do
      MockServer.pause()
      reanalyze_data_source(ds1)

      data_source_id = ds1.id

      assert_soon(
        [
          %{
            stats: %{
              complete: 0,
              processing: 2,
              error: 0,
              total: 2
            },
            id: ^data_source_id
          },
          _
        ] = Explorer.all_data_source_metadata()
      )

      MockServer.resume()

      assert_soon(
        [
          %{
            stats: %{
              complete: 1,
              processing: 0,
              error: 1,
              total: 2
            },
            id: ^data_source_id
          },
          _
        ] = Explorer.all_data_source_metadata()
      )
    end

    test "excludes tables that can't be analyzed", %{ds1: ds1} do
      DataSource.update!(ds1, %{tables: Jason.encode!([@bazs_table])})

      assert_soon(is_nil(metadata_for_data_source(ds1)))
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
      assert :ok = Explorer.setup_credentials_if_required()
      assert {:ok, _} = Air.Service.User.get_by_login("diffix-explorer@aircloak.com")
      assert {:ok, _} = Air.Service.Group.get_by_name("Diffix Explorer")
    end

    test "is idempotent" do
      assert :ok = Explorer.setup_credentials_if_required()
      users = Air.Service.User.all()
      assert :ok = Explorer.setup_credentials_if_required()
      assert users == Air.Service.User.all()
    end
  end

  describe ".data_source_updated" do
    test "soft deletes results if table no longer exists in the data source, and undoes it too", context do
      reanalyze_data_source(context.ds1)

      assert_soon [
                    %ExplorerAnalysis{table_name: "bars", soft_delete: false},
                    %ExplorerAnalysis{table_name: "foos", soft_delete: false}
                  ] = sorted_results_for_data_source(context.ds1)

      DataSource.update!(context.ds1, %{
        tables: Jason.encode!([@bars_table])
      })

      assert_soon [
                    %ExplorerAnalysis{table_name: "bars", soft_delete: false},
                    %ExplorerAnalysis{table_name: "foos", soft_delete: true}
                  ] = sorted_results_for_data_source(context.ds1)

      DataSource.update!(context.ds1, %{
        tables: Jason.encode!([@bars_table, @foos_table])
      })

      assert_soon [
                    %ExplorerAnalysis{table_name: "bars", soft_delete: false},
                    %ExplorerAnalysis{table_name: "foos", soft_delete: false}
                  ] = sorted_results_for_data_source(context.ds1)
    end

    test "soft deleted tables are not part of the metadata", context do
      reanalyze_data_source(context.ds1)
      data_source_name = context.ds1.name

      assert_soon(
        %{
          tables: [%{name: "bars"}, %{name: "foos"}],
          name: ^data_source_name
        } = metadata_for_data_source(context.ds1)
      )

      DataSource.update!(context.ds1, %{
        tables: Jason.encode!([@bars_table])
      })

      assert_soon(
        %{
          tables: [%{name: "bars"}],
          name: ^data_source_name
        } = metadata_for_data_source(context.ds1)
      )
    end
  end

  describe ".analyze_datasource" do
    test "creates jobs and polls for results", context do
      MockServer.pause()
      assert :ok == reanalyze_data_source(context.ds1)

      assert_soon(
        [
          %ExplorerAnalysis{table_name: "bars", status: :processing},
          %ExplorerAnalysis{table_name: "foos", status: :processing}
        ] = sorted_results_for_data_source(context.ds1),
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
        ] = sorted_results_for_data_source(context.ds1),
        timeout: 500
      )
    end
  end

  describe "adds permission as necessary" do
    test "analyzing a data source adds it to the explorer group", context do
      data_source = context.ds_not_included
      refute Enum.any?(Explorer.group().data_sources, &(&1.id == data_source.id))

      reanalyze_data_source(data_source)
      assert_soon Enum.any?(Explorer.group().data_sources, &(&1.id == data_source.id))
    end

    test "it adds results for newly authorized data sources", context do
      data_source = context.ds_not_included
      data_source_name = data_source.name

      assert(
        %{
          tables: [%{name: "bars", status: :not_enabled}, %{name: "foos", status: :not_enabled}]
        } = metadata_for_data_source(data_source)
      )

      MockServer.pause()
      reanalyze_data_source(data_source)

      assert_soon(
        %{
          tables: [%{name: "bars", status: :processing}, %{name: "foos", status: :processing}],
          name: ^data_source_name
        } = metadata_for_data_source(data_source),
        timeout: 500
      )

      MockServer.resume()
    end

    test "it does not attempt to get results for already existing data sources", context do
      # Setup: make sure we have ds1 one ready and results already fetched
      reanalyze_data_source(context.ds1)

      assert_soon(
        [
          %ExplorerAnalysis{
            table_name: "bars",
            status: :error
          },
          %ExplorerAnalysis{}
        ] = sorted_results_for_data_source(context.ds1),
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

      reanalyze_data_source(context.ds_not_included)

      # Here we wait for ds_not_included to return from polling
      assert_soon(
        [
          %ExplorerAnalysis{
            table_name: "bars",
            status: :processing
          },
          %ExplorerAnalysis{}
        ] = sorted_results_for_data_source(context.ds_not_included),
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
        ] = sorted_results_for_data_source(context.ds1),
        timeout: 500
      )
    end
  end

  defp reanalyze_data_source(data_source),
    do:
      data_source
      |> Air.Schemas.DataSource.tables()
      |> Enum.each(&Explorer.analyze_table(data_source.id, &1["id"]))

  defp metadata_for_data_source(data_source),
    do:
      Explorer.all_data_source_metadata()
      |> Enum.find(&(&1.id == data_source.id))

  defp sorted_results_for_data_source(data_source),
    do:
      data_source
      |> Explorer.results_for_datasource()
      |> Enum.sort_by(& &1.table_name)
end

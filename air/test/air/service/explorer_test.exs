defmodule Air.Service.DataSourceTest do
  # because of shared mode
  use Air.SchemaCase, async: false
  alias Air.Service.Explorer
  alias Air.Schemas.ExplorerAnalysis
  require Aircloak.DeployConfig

  defmodule MockServer do
    defmodule Controller do
      use Phoenix.Controller, namespace: AirWeb

      def explore(conn, params) do
        json(conn, %{
          status: "New",
          metrics: [],
          id: params["ColumnName"]
        })
      end

      def result(conn, %{"id" => id}) do
        case id do
          "foo" ->
            json(conn, %{
              status: "Complete",
              metrics: [%{value: [32], key: "some-metric"}],
              id: "foo"
            })

          "user_id" ->
            resp(conn, 500, "Something went wrong")
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

  setup do
    endpoint = MockServer.Endpoint.start_link(false)

    tables = [
      %{
        columns: [
          %{
            name: "user_id",
            user_id: true
          },
          %{
            name: "foo",
            user_id: false
          }
        ],
        id: "foos"
      }
    ]

    ds1 = Air.TestRepoHelper.create_data_source!(%{tables: Jason.encode!(tables)})

    ds_not_included = Air.TestRepoHelper.create_data_source!(%{tables: Jason.encode!(tables)})

    Aircloak.DeployConfig.update("explorer", fn _ ->
      %{"url" => MockServer.Endpoint.url() <> "/explorer", "api_key" => "foobar", "data_sources" => [ds1.name]}
    end)

    %{endpoint: endpoint, ds1: ds1, ds_not_included: ds_not_included}
  end

  describe ".enabled?" do
    test "returns true when config exists" do
      # No idea how to test false, since the config appears pretty static...
      assert Explorer.enabled?()
    end
  end

  describe ".data_source_supported?" do
    test "returns true if source included in config", context do
      assert Explorer.data_source_supported?(context.ds1)
    end

    test "returns false if source not included in config", context do
      assert not Explorer.data_source_supported?(context.ds_not_included)
    end
  end

  describe ".reanalyze_datasource" do
    test "creates analysis records", context do
      assert :ok == Explorer.reanalyze_datasource(context.ds1)

      assert [
               %ExplorerAnalysis{table_name: "foos", column: "user_id", status: :new},
               %ExplorerAnalysis{table_name: "foos", column: "foo", status: :new}
             ] = Explorer.results_for_datasource(context.ds1)

      # This prevents errors from terminating DB connexion
      Process.sleep(100)
    end

    test "begins polling for results", context do
      Explorer.reanalyze_datasource(context.ds1)
      Process.sleep(100)

      assert [
               %ExplorerAnalysis{table_name: "foos", column: "user_id", status: :error, metrics: "[]"},
               %ExplorerAnalysis{
                 table_name: "foos",
                 column: "foo",
                 status: :complete,
                 metrics: "[{\"key\":\"some-metric\",\"value\":[32]}]"
               }
             ] = Explorer.results_for_datasource(context.ds1)
    end

    test "removes old records", context do
      Explorer.reanalyze_datasource(context.ds1)
      Process.sleep(100)
      results = Explorer.results_for_datasource(context.ds1)
      refute results == Explorer.reanalyze_datasource(context.ds1)
      # This prevents errors from terminating DB connexion
      Process.sleep(100)
    end
  end
end

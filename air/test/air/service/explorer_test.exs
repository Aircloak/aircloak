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
    endpoint = MockServer.Endpoint.start_link([])

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

    Aircloak.DeployConfig.update("explorer", fn _ ->
      %{"url" => MockServer.Endpoint.url() <> "/explorer", "api_key" => "foobar", "data_sources" => [ds1.name]}
    end)

    %{endpoint: endpoint, ds1: ds1}
  end

  test ".begin_analyses" do
    assert :ok == Explorer.begin_analyses()

    assert [
             %ExplorerAnalysis{table_name: "foos", column: "user_id", status: :new},
             %ExplorerAnalysis{table_name: "foos", column: "foo", status: :new}
           ] = Explorer.all()
  end

  test ".poll_for_updates" do
    Explorer.begin_analyses()
    Process.sleep(10)
    assert :ok == Explorer.poll_for_updates()

    assert [
             %ExplorerAnalysis{table_name: "foos", column: "user_id", status: :error, metrics: "[]"},
             %ExplorerAnalysis{
               table_name: "foos",
               column: "foo",
               status: :complete,
               metrics: "[{\"key\":\"some-metric\",\"value\":[32]}]"
             }
           ] = Explorer.all()
  end
end

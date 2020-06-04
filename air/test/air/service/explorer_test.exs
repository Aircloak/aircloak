defmodule Air.Service.ExplorerTest do
  # because of shared mode
  use Air.SchemaCase, async: false
  alias Air.Service.{Explorer, Group}
  alias Air.Schemas.ExplorerAnalysis
  require Aircloak.DeployConfig
  import Aircloak.AssertionHelper

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

  describe ".reanalyze_datasource" do
    test "creates analysis records, polls for results and removes old records", context do
      assert :ok == Explorer.reanalyze_datasource(context.ds1)
      results = Enum.sort_by(Explorer.results_for_datasource(context.ds1), & &1.column)

      assert [
               %ExplorerAnalysis{table_name: "foos", column: "bar", status: :new},
               %ExplorerAnalysis{table_name: "foos", column: "foo", status: :new}
             ] = results

      # Here we wait for polling to happen
      assert soon(
               match?(
                 [
                   %ExplorerAnalysis{table_name: "foos", column: "bar", status: :error, metrics: "[]"},
                   %ExplorerAnalysis{
                     table_name: "foos",
                     column: "foo",
                     status: :complete,
                     metrics: "[{\"key\":\"some-metric\",\"value\":[32]}]"
                   }
                 ],
                 Enum.sort_by(Explorer.results_for_datasource(context.ds1), & &1.column)
               )
             )

      Explorer.reanalyze_datasource(context.ds1)
      refute results == Explorer.results_for_datasource(context.ds1)

      # This prevents "Client is still using a connection from owner at location"
      # errors from appearing in the test log. These errors don't seem to break anything,
      # but make test output unnecesarily noisy.
      Process.sleep(50)
    end
  end

  defmodule MockServer do
    @moduledoc "Mocks the [Diffix Explorer API](https://github.com/diffix/explorer#usage)."
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

          "bar" ->
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
    config = Application.get_env(:air, Aircloak.DeployConfig)
    start_supervised!(MockServer.Endpoint)

    Application.put_env(
      :air,
      Aircloak.DeployConfig,
      Map.put(config, "explorer", %{"url" => MockServer.Endpoint.url() <> "/explorer"})
    )

    on_exit(fn -> Application.put_env(:air, Aircloak.DeployConfig, config) end)

    start_supervised!(Explorer)

    tables = [
      %{
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
          },
          %{
            name: "bar",
            user_id: false,
            isolated: false
          }
        ],
        id: "foos"
      }
    ]

    ds1 = Air.TestRepoHelper.create_data_source!(%{tables: Jason.encode!(tables)})

    ds_not_included = Air.TestRepoHelper.create_data_source!(%{tables: Jason.encode!(tables)})
    Explorer.setup_credentials_if_required()
    {:ok, group} = Group.get_by_name("Diffix Explorer")
    group = Group.load(group.id)
    Group.update!(group, %{data_sources: [ds1.id]})

    %{ds1: ds1, ds_not_included: ds_not_included}
  end
end

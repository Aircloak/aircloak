defmodule Mix.Tasks.ConfigSapHanaTestSchema do
  @moduledoc """
  Generates local configuration with a random test schema for SAP HANA.

  This task can only work in CI builds, and it helps ensuring that different builds use different schemas.
  This helps us to avoid collisions, while at the same time it allows us to use the same schema within a single Travis
  build. This is critical for the compliance tests, where we need to populate the data with a `gen.test_data` mix task,
  and we need to query that data from the tests.
  """

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  use Mix.Task

  @impl Mix.Task
  def run(_args) do
    if System.get_env("CI") == "true" do
      schema_name = "test_schema_#{Base.encode16(:crypto.strong_rand_bytes(10))}"

      IO.puts "configured SAP HANA schema `#{schema_name}`"

      local_config =
        """
          use Mix.Config
          config :cloak, :sap_hana, default_schema: "#{schema_name}"
        """

      Enum.each(
        [:dev, :test],
        &File.write!("#{File.cwd!()}/config/#{&1}.local.exs", local_config)
      )
    end
  end
end

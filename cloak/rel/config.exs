# Import all plugins from `rel/plugins`
# They can then be used by adding `plugin MyPlugin` to
# either an environment, or release definition, where
# `MyPlugin` is the name of the plugin module.
Path.join(["rel", "plugins", "*.exs"])
|> Path.wildcard()
|> Enum.map(&Code.eval_file(&1))

use Mix.Releases.Config,
  # This sets the default release built by `mix release`
  default_release: :cloak,
  # This sets the default environment used by `mix release`
  default_environment: :prod

environment :prod do
  set(include_erts: true)
  set(include_src: false)
  set(cookie: "cloak")

  plugin(
    Aircloak.Release.Adjust,
    keep_only: [cloak: [{"priv", ~w(config_schema.json datasource_schema.json native odbc)}]],
    remove: [cloak: [{"priv/odbc/drivers", ~w(saphana)}]]
  )
end

environment :local do
  set(dev_mode: true)
  set(include_erts: true)
  set(cookie: "cloak")
  set(config: "config/local_release.exs")
end

release :cloak do
  set(version: current_version(:cloak))
  set(erl_opts: "+K true")
end

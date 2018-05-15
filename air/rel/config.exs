# Import all plugins from `rel/plugins`
# They can then be used by adding `plugin MyPlugin` to
# either an environment, or release definition, where
# `MyPlugin` is the name of the plugin module.
Path.join(["rel", "plugins", "*.exs"])
|> Path.wildcard()
|> Enum.map(&Code.eval_file(&1))

use Mix.Releases.Config,
  # This sets the default release built by `mix release`
  default_release: :air,
  # This sets the default environment used by `mix release`
  default_environment: :prod

environment :prod do
  set(include_erts: true)
  set(include_src: false)
  set(cookie: "air")
end

environment :local do
  set(dev_mode: true)
  set(include_erts: true)
  set(cookie: "air")
  set(config: "config/local_release.exs")
end

release :air do
  set(version: current_version(:air))
  set(erl_opts: "+K true")

  set(
    commands: [
      reset_password: "rel/commands/reset_password.sh"
    ]
  )
end

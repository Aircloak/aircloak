Path.join(["rel", "plugins", "*.exs"])
|> Path.wildcard()
|> Enum.map(&Code.eval_file(&1))

use Mix.Releases.Config,
  default_release: :aircloak_ci,
  default_environment: :prod

environment :prod do
  set include_erts: true
  set include_src: false
  set cookie: :aircloak_ci
end

release :aircloak_ci do
  set version: current_version(:aircloak_ci)
  set erl_opts: "+K true"
end

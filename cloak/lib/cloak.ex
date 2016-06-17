defmodule Cloak do
  @moduledoc false
  use Application

  # See http://elixir-lang.org/docs/stable/elixir/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    Cloak.DeployConfig.load()

    Supervisor.start_link(children(), strategy: :one_for_one, name: Cloak.Supervisor)
  end

  # Conditional definition of top-level processes, since we don't want to run
  # all of them in the test environment.
  case Mix.env do
    :test -> defp children, do: common_processes()
    :dev -> defp children, do: common_processes() ++ system_processes()
    :prod -> defp children, do: common_processes() ++ system_processes()
  end

  defp common_processes do
    import Supervisor.Spec, warn: false

    [
      supervisor(Cloak.DataSource, []),
      supervisor(:result_sender_sup, []),
      Cloak.Query.supervisor_spec()
    ]
  end

  unless Mix.env == :test do
    # Processes which we don't want to start in the test environment
    defp system_processes do
      import Supervisor.Spec, warn: false

      [
        worker(Cloak.AirSocket, [])
      ]
    end
  end
end

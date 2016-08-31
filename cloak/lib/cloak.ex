defmodule Cloak do
  @moduledoc false
  use Application

  # See http://elixir-lang.org/docs/stable/elixir/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    set_salt()
    :ok = Cloak.DataSource.start()
    Supervisor.start_link(children(), strategy: :one_for_one, name: Cloak.Supervisor)
  end

  defp set_salt() do
    salt = case Cloak.DeployConfig.fetch("salt") do
      :error -> "default salt"
      {:ok, value} -> value
    end
    existing_env = Application.get_env(:cloak, :anonymizer)
    new_env = Keyword.put(existing_env, :salt, salt)
    Application.put_env(:cloak, :anonymizer, new_env)
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
      Cloak.ResultSender.supervisor_spec(),
      Cloak.Query.Runner.supervisor_spec()
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

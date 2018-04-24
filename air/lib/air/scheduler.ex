defmodule Air.Scheduler do
  use Quantum.Scheduler, otp_app: :air

  def child_spec(_arg), do: Supervisor.Spec.worker(__MODULE__, [])
end

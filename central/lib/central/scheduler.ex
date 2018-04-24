defmodule Central.Scheduler do
  use Quantum.Scheduler, otp_app: :central

  def child_spec(_arg), do: Supervisor.Spec.worker(__MODULE__, [])
end

defmodule Cloak.Scheduler do
  use Quantum.Scheduler, otp_app: :cloak

  def child_spec(_arg), do: Supervisor.Spec.worker(__MODULE__, [])
end

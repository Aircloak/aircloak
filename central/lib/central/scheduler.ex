defmodule Central.Scheduler do
  # Quantum.Scheduler defines the moduledoc and Credo cannot detect that
  # credo:disable-for-this-file Credo.Check.Readability.ModuleDoc

  use Quantum.Scheduler, otp_app: :central

  def child_spec(_arg), do: Supervisor.Spec.worker(__MODULE__, [])
end

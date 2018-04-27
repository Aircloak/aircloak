defmodule Central.Scheduler do
  # Quantum.Scheduler defines the moduledoc and Credo cannot detect that
  # credo:disable-for-this-file Credo.Check.Readability.ModuleDoc

  use Quantum.Scheduler, otp_app: :central
end

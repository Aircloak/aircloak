defmodule IntegrationTest.AcceptanceCase do
  use ExUnit.CaseTemplate

  using do
    quote do
      use Hound.Helpers
      import IntegrationTest.AcceptanceHelper
      alias IntegrationTest.Manager

      setup_all do
        if System.get_env("CI") in [nil, ""] and System.get_env("AIR_IP") in [nil, ""],
          do: raise("Set AIR_IP OS environment variable to the local network IP of your machine.")

        :ok
      end

      setup do
        Hound.start_session()
        parent = self()
        on_exit(fn -> Hound.end_session(parent) end)

        set_window_size(current_window_handle(), 1024, 768)

        :ok
      end

      @moduletag :acceptance
    end
  end
end

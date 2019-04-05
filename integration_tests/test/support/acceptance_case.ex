defmodule IntegrationTest.AcceptanceCase do
  use ExUnit.CaseTemplate

  using do
    quote do
      use Hound.Helpers
      import IntegrationTest.AcceptanceHelper
      alias IntegrationTest.Manager

      setup do
        Hound.start_session()
        parent = self()
        on_exit(fn -> Hound.end_session(parent) end)

        set_window_size(current_window_handle(), 1920, 1080)

        :ok
      end

      @moduletag :acceptance
    end
  end
end

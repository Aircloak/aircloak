defmodule IntegrationTest.AcceptanceCase do
  use ExUnit.CaseTemplate

  using do
    quote do
      use Wallaby.DSL
      import Wallaby.Query, only: [css: 1, css: 2, xpath: 1]
      import IntegrationTest.AcceptanceHelper
      alias IntegrationTest.Manager

      @moduletag :acceptance
    end
  end
end

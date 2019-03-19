defmodule IntegrationTest.AcceptanceCase do
  use ExUnit.CaseTemplate

  using do
    quote do
      use Wallaby.DSL
      import IntegrationTest.AcceptanceHelper
      alias IntegrationTest.Manager
    end
  end
end

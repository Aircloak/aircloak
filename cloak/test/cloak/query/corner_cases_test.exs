defmodule Cloak.Query.CornerCase.Test do
  use ExUnit.Case, async: true

  import Cloak.Test.QueryHelpers

  setup_all do
    :ok = Cloak.Test.DB.create_table("corner_cases", "dummy INTEGER")
  end

  test "[Issue #2568] Expression to be sent to DB evaluates to nil" do
    assert_query("SELECT count(*) FROM corner_cases WHERE sqrt(-1) = 0", %{rows: [%{row: [0]}]})
  end
end

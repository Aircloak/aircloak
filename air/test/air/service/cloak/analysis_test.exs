defmodule Air.Service.Cloak.Analysis.Test do
  use ExUnit.Case, async: true

  alias Air.Service.Cloak.Analysis

  setup do
    Analysis.start_link({})
    :ok
  end

  test "can update the registry" do
    assert Analysis.available() == []
    t = NaiveDateTime.utc_now()
    Analysis.complete([%{result: "efqe43", descriptor: "3ef3f3qf4", type: "isolator", status: :success, expires: t}])

    assert Analysis.available() == [
             %{result: "efqe43", descriptor: "3ef3f3qf4", type: "isolator", status: :success, expires: t}
           ]
  end
end

defmodule Air.Service.Cloak.AnalysisCache.Test do
  use ExUnit.Case, async: true

  alias Air.Service.Cloak.AnalysisCache

  setup do
    AnalysisCache.start_link({})
    :ok
  end

  test "can update the registry" do
    assert AnalysisCache.all() == []
    t = NaiveDateTime.utc_now()
    Analysis.insert([%{result: "efqe43", descriptor: "3ef3f3qf4", type: "isolator", status: :success, expires: t}])

    assert Analysis.all() == [
             %{result: "efqe43", descriptor: "3ef3f3qf4", type: "isolator", status: :success, expires: t}
           ]
  end
end

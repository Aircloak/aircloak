defmodule Cloak.Encoding.Test do
  use ExUnit.Case, async: true

  test "encoding an interval as JSON" do
    assert {:ok, "\"P10DT10M\""} = Poison.encode(Timex.Duration.parse!("P10DT10M"))
  end
end

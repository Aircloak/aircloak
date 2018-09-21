defmodule DataQuality.Stats.Test do
  use ExUnit.Case

  alias DataQuality.Stats

  describe ".mean_square_error" do
    test "returns 0 when there are no values", do: assert(Stats.mean_squared_error([]) == 0.0)

    test "returns correct mean squared error",
      do:
        assert(
          Stats.mean_squared_error([
            {1, 1},
            {2, 2},
            {3, 3},
            {1, 0},
            {3, 0}
          ]) == 2.0
        )

    test "ignores null-values",
      do:
        assert(
          Stats.mean_squared_error([
            {3, 0},
            {100, nil}
          ]) == 9.0
        )
  end
end

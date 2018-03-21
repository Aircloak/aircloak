defmodule Cloak.Sql.NoiseLayer.Normalizer.Test do
  use ExUnit.Case, async: true

  alias Cloak.Sql.NoiseLayer.Normalizer

  describe "floats" do
    test "normalizing an integer", do:
      assert same(6, 6.0)

    test "same after normalization if difference in insignificant digits" do
      assert same(1.1231231, 1.123123123)
      assert same(12341234.123123123, 12341234.123123124)
    end

    test "should not conflate similar looking numbers" do
      refute same(0.1123123, 1.123456)
      refute same(11.23123, 1.123456)
      refute same(-0.1123123, -1.123456)
      refute same(-11.23123, -1.123456)
      refute same(0.1123123, -1.123456)
      refute same(-0.1123123, 1.123456)
      refute same(11.23123, -1.123456)
      refute same(-11.23123, 1.123456)
    end

    test "should not conflate a positive and the same negative number", do:
      refute same(0.1123123, -0.1123456)

    test "normalizes 0.0 as well", do:
      assert same(0.0, -0.0)
  end

  defp same(num1, num2), do:
    Normalizer.normalize_number(num1) == Normalizer.normalize_number(num2)
end

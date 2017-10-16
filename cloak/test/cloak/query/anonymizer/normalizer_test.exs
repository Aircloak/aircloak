defmodule Cloak.Query.Anonimyzer.Normalizer.Test do
  use ExUnit.Case, async: true

  alias Cloak.Query.Anonymizer.Normalizer

  describe "floats" do
    test "normalizing an integer", do:
      assert same(6, 6.0)

    test "same after normalization if difference in insignificant digits", do:
      assert same(1.123123, 1.123456)

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

    test "should return the same number of significant digits irrespective of internal alterations" do
      {{number1, exponent1}, _} = Normalizer.normalize_number(1.234, 3)
      {{number2, exponent2}, _} = Normalizer.normalize_number(12.34, 3)
      assert number1 == number2
      refute exponent1 == exponent2
    end

    test "normalizes 0.0 as well", do:
      {{_, _}, _} = Normalizer.normalize_number(0.0, 3)
  end

  defp same(num1, num2), do:
    Normalizer.normalize_number(num1, 3) == Normalizer.normalize_number(num2, 3)
end

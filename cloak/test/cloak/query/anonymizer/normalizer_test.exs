defmodule Cloak.Query.Anonimyzer.Normalizer.Test do
  use ExUnit.Case, async: true

  alias Cloak.Query.Anonymizer.Normalizer

  describe "floats" do
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
      normalized = Normalizer.normalize_float(12.34, 1)
      # When 12.34 was normalized to have one place before the decimal point,
      # it became 1.234. We however wanted to retain 1 significant digit of
      # the original number (i.e. the 3). The final normalized value then
      # became XXX1.23, where XXX is some cruft added in to make the number
      # distinct from other similar values.
      assert rem(trunc(normalized * 100), 10) == 3
      assert rem(trunc(normalized * 1000), 10) == 0
    end
  end

  defp same(num1, num2), do:
    Normalizer.normalize_float(num1, 3) == Normalizer.normalize_float(num2, 3)
end

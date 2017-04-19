defmodule Cloak.DataSource.Validations.Name.Test do
  use ExUnit.Case, async: true

  describe "ensure_permitted" do
    test "allows valid names", do:
      assert errors_for_name("valid_name") == []

    test "add error on too long name", do:
      assert_name_produces_error("this_name_exceeds_31_characters_in_length", ~r/too long/)

    test "add error on invalid character", do:
      assert_name_produces_error("invalid!name", ~r/alphanumeric characters/)

    test "add error on invalid first character", do:
      assert_name_produces_error("1invalid!name", ~r/must start with/)

    Enum.each(Cloak.Sql.Lexer.keywords(), fn(keyword) ->
      test "add error on usage of restricted keyword (#{keyword})" do
        assert_name_produces_error(unquote(keyword), ~r/reserved SQL keyword/)
        assert_name_produces_error(String.downcase(unquote(keyword)), ~r/reserved SQL keyword/)
      end
    end)
  end

  defp assert_name_produces_error(name, error), do:
    assert hd(errors_for_name(name)) =~ error

  defp errors_for_name(name) do
    data_source = %{name: name, errors: []}
    %{errors: errors} = Cloak.DataSource.Validations.Name.ensure_permitted(data_source)
    errors
  end
end

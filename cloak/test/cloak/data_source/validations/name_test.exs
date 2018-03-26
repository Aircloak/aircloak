defmodule Cloak.DataSource.Validations.Name.Test do
  use ExUnit.Case, async: true

  alias Cloak.DataSource.Validations.Name

  describe "ensure_permitted" do
    test "allows valid names", do: assert(errors_for_name("valid_name") == [])

    test "add error on missing name" do
      data_source = data_source_config()
      %{errors: errors} = Name.ensure_permitted(data_source)
      assert hd(errors) =~ ~r/needs to be configured with a name/
    end

    test "add error on blank name" do
      data_source = data_source_config("")
      %{errors: errors} = Name.ensure_permitted(data_source)
      assert hd(errors) =~ ~r/needs to be configured with a name/
    end

    test "add error on too long name",
      do: assert_name_produces_error("this_name_exceeds_31_characters_in_length", ~r/too long/)

    test "add error on invalid character",
      do: assert_name_produces_error("invalid!name", ~r/alphanumeric characters/)

    test "add error on invalid first character",
      do: assert_name_produces_error("1invalid!name", ~r/must start with/)

    Enum.each(Cloak.Sql.Lexer.keywords(), fn keyword ->
      test "add error on usage of restricted keyword (#{keyword})" do
        assert_name_produces_error(unquote(keyword), ~r/reserved SQL keyword/)
        assert_name_produces_error(String.downcase(unquote(keyword)), ~r/reserved SQL keyword/)
      end
    end)
  end

  describe "check_for_duplicates" do
    test "no change on all valid data sources" do
      data_sources = for n <- 1..10, do: data_source_config("name-#{n}")
      assert Name.check_for_duplicates(data_sources) == data_sources
    end

    test "retains data source order" do
      data_sources = for n <- 1..10, do: data_source_config("name-#{n}")
      data_sources = data_sources ++ data_sources
      validated_datasource = Name.check_for_duplicates(data_sources)

      for {original, validated} <- Enum.zip(data_sources, validated_datasource),
          do: assert(original.name == validated.name)
    end

    test "adds errors on data sources with the same name" do
      data_sources = [
        data_source_config("name1"),
        data_source_config("name2"),
        data_source_config("name2")
      ]

      [_valid, %{errors: [e1]}, %{errors: [e2]}] = Name.check_for_duplicates(data_sources)
      assert e1 == e2
      assert e1 =~ ~r/duplicate entries for .* name2/is
    end
  end

  defp assert_name_produces_error(name, error), do: assert(hd(errors_for_name(name)) =~ error)

  defp errors_for_name(name) do
    data_source = data_source_config(name)
    %{errors: errors} = Name.ensure_permitted(data_source)
    errors
  end

  defp data_source_config(name \\ nil) do
    data_source = %{name: name, errors: [], parameters: %{host: "host", database: "database"}}

    if is_nil(name) do
      Map.delete(data_source, :name)
    else
      data_source
    end
  end
end

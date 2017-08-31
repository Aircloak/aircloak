defmodule Cloak.Sql.Compiler.VerificationSelectableColumnsValidTransformations.Test do
  use ExUnit.Case, async: true

  alias Cloak.DataSource.Table

  import Cloak.Test.QueryHelpers

  describe "rejects queries selecing columns that have seen math, discontinuity and constants" do
    Enum.each(~w(+ - * ^), fn(math_function) ->
      Enum.each(~w(abs ceil floor round trunc), fn(discontinuous_function) ->
        test "when on the same level ('#{math_function}' and '#{discontinuous_function}')" do
          query = """
            SELECT #{unquote(discontinuous_function)}(numeric #{unquote(math_function)} 3)
            FROM table
          """
          refute select_columns_have_valid_transformations(query)
        end

        test "when across queries ('#{math_function}' and '#{discontinuous_function}')" do
          query = """
            SELECT #{unquote(discontinuous_function)}(num)
            FROM (
              SELECT uid, numeric #{unquote(math_function)} 3 as num
              FROM table
            ) t
          """
          refute select_columns_have_valid_transformations(query)
        end

        test "ok when no constant ('#{math_function}' and '#{discontinuous_function}')" do
          query = """
            SELECT #{unquote(discontinuous_function)}(numeric #{unquote(math_function)} numeric)
            FROM table
          """
          assert select_columns_have_valid_transformations(query)
        end

        test "ok when math is only with constants ('#{math_function}' and '#{discontinuous_function}')" do
          query = """
            SELECT #{unquote(discontinuous_function)}(1 #{unquote(math_function)} 3)
            FROM table
          """
          assert select_columns_have_valid_transformations(query)
        end
      end)

      Enum.each(~w(div mod), fn(discontinuous_function) ->
        test "when on the same level ('#{math_function}' and '#{discontinuous_function}')" do
          query = """
            SELECT #{unquote(discontinuous_function)}(numeric, 2) #{unquote(math_function)} 3
            FROM table
          """
          refute select_columns_have_valid_transformations(query)
        end

        test "when across queries ('#{math_function}' and '#{discontinuous_function}')" do
          query = """
            SELECT num #{unquote(math_function)} 3
            FROM (
              SELECT uid, #{unquote(discontinuous_function)}(numeric, 2) as num
              FROM table
            ) t
          """
          refute select_columns_have_valid_transformations(query)
        end

        test "ok when math is only with constants ('#{math_function}' and '#{discontinuous_function}')" do
          query = """
            SELECT #{unquote(discontinuous_function)}(1, 2) #{unquote(math_function)} 3
            FROM table
          """
          assert select_columns_have_valid_transformations(query)
        end
      end)

      test "when on the same level ('#{math_function}' and 'bucket')" do
        query = """
          SELECT bucket(numeric by 10) #{unquote(math_function)} 3
          FROM table
        """
        refute select_columns_have_valid_transformations(query)
      end

      test "when across queries ('#{math_function}' and 'bucket')" do
        query = """
          SELECT num #{unquote(math_function)} 3
          FROM (
            SELECT uid, bucket(numeric by 10) as num
            FROM table
          ) t
        """
        refute select_columns_have_valid_transformations(query)
      end

      test "when on the same level ('#{math_function}' and '%')" do
        query = """
          SELECT (numeric % 7) #{unquote(math_function)} 3
          FROM table
        """
        refute select_columns_have_valid_transformations(query)
      end

      test "when across queries ('#{math_function}' and '%')" do
        query = """
          SELECT num #{unquote(math_function)} 3
          FROM (
            SELECT uid, numeric % 7 as num
            FROM table
          ) t
        """
        refute select_columns_have_valid_transformations(query)
      end
    end)
  end

  describe "casts are dangerously discontinuous when what is cast has been influenced by a constant" do
    Enum.each(~w(real boolean), fn(cast_target) ->
      test "cast from integer to #{cast_target}" do
        query = "SELECT cast(numeric + 1 as #{unquote(cast_target)}) FROM table"
        refute select_columns_have_valid_transformations(query)
      end
    end)

    test "cast from float to integer" do
      query = "SELECT cast(float + 1 as integer) FROM table"
      refute select_columns_have_valid_transformations(query)
    end

    # Testing that casts from text is dangerously discontinuous if a constant
    # has been involved, since that would involve a distinct discontinuous
    # function first, which means we aren't testing whether the cast is what
    # triggered the discontinuity flag or not. Hence no tests for:
    # cast(text to datetime | time | date | text | integerval)
  end

  defp select_columns_have_valid_transformations(query) do
    case compile(query, data_source()) do
      {:ok, _} -> true
      {:error, reason} ->
        if reason =~ ~r/is influenced by math and a discontinuous function/ do
          false
        else
          raise "Compilation failed with other reason than illegal math/discontinuity: #{inspect reason}"
        end
    end
  end

  defp data_source() do
    %{
      driver: Cloak.DataSource.PostgreSQL,
      tables: %{
        table: Cloak.DataSource.Table.new("table", "uid",
          db_name: "table",
          columns: [
            Table.column("uid", :integer),
            Table.column("column", :datetime),
            Table.column("numeric", :integer),
            Table.column("float", :real),
            Table.column("string", :text)
          ]
        )
      }
    }
  end
end

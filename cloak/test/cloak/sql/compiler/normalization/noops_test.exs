defmodule Cloak.Sql.Compiler.Normalization.Noops.Test do
  use ExUnit.Case, async: true

  alias Cloak.DataSource.Table

  import Cloak.Test.QueryHelpers

  describe "remove" do
    test "a cast of integer to integer" do
      result1 = remove_noops!("SELECT * FROM table WHERE cast(numeric AS integer) = 1", data_source())

      result2 = remove_noops!("SELECT * FROM table WHERE numeric = 1", data_source())

      assert scrub_locations(result1).where == scrub_locations(result2).where
    end

    for function <- ~w/round trunc/ do
      test "#{function} of integer without precision is removed" do
        result1 =
          remove_noops!(
            "SELECT * FROM table WHERE #{unquote(function)}(numeric) = 1",
            data_source()
          )

        result2 = remove_noops!("SELECT * FROM table WHERE numeric = 1", data_source())

        assert scrub_locations(result1).where == scrub_locations(result2).where
      end

      test "#{function} of integer with precision isn't removed" do
        result1 =
          remove_noops!(
            "SELECT * FROM table WHERE #{unquote(function)}(numeric, 0) = 1",
            data_source()
          )

        result2 = remove_noops!("SELECT * FROM table WHERE numeric = 1", data_source())

        refute scrub_locations(result1).where == scrub_locations(result2).where
      end
    end

    for function <- ~w/ceil ceiling floor/ do
      test "#{function} of integer is removed" do
        result1 =
          remove_noops!(
            "SELECT * FROM table WHERE #{unquote(function)}(numeric) = 1",
            data_source()
          )

        result2 = remove_noops!("SELECT * FROM table WHERE numeric = 1", data_source())

        assert scrub_locations(result1).where == scrub_locations(result2).where
      end
    end
  end

  defp remove_noops!(query, data_source, parameters \\ [], views \\ %{}) do
    {:ok, parsed} = Cloak.Sql.Parser.parse(query)

    parsed
    |> Cloak.Sql.Compiler.Specification.compile(data_source, parameters, views)
    |> Cloak.Sql.Compiler.Validation.verify_query()
    |> Cloak.Sql.Compiler.Normalization.Noops.remove()
  end

  defp data_source() do
    %{
      name: "normalization_data_source",
      driver: Cloak.DataSource.PostgreSQL,
      tables: %{
        table:
          Cloak.DataSource.Table.new(
            "table",
            "uid",
            db_name: "table",
            columns: [
              Table.column("uid", :integer),
              Table.column("numeric", :integer),
              Table.column("string", :text)
            ],
            keys: ["key"]
          )
      }
    }
  end
end

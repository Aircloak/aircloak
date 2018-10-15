defmodule Cloak.DataSource.Shadows.Query.Test do
  use ExUnit.Case, async: false

  import Cloak.Test.QueryHelpers
  alias Cloak.DataSource
  alias Cloak.DataSource.Shadows.Query

  setup_all do
    :ok =
      Cloak.Test.DB.create_table("shadows", "value INTEGER, encoded_value TEXT",
        decoders: [%{method: "base64", columns: ["encoded_value"]}]
      )

    :ok = Cloak.Test.DB.create_table("shadows_userless", "value INTEGER", user_id: nil)
  end

  setup do
    :ok = Cloak.Test.DB.clear_table("shadows")
  end

  describe "build_shadow" do
    test "popular values are returned" do
      :ok = insert_rows(_user_ids = 0..20, "shadows", ["value"], [10])
      :ok = insert_rows(_user_ids = 0..20, "shadows", ["value"], [20])

      for data_source <- DataSource.all() do
        assert [10, 20] = Query.build_shadow(data_source, "shadows", "value")
      end
    end

    test "values with < 10 users are ignored" do
      :ok = insert_rows(_user_ids = 0..20, "shadows", ["value"], [10])
      :ok = insert_rows(_user_ids = 0..0, "shadows", ["value"], [30])

      for _ <- 1..20 do
        :ok = insert_rows(_user_ids = 0..0, "shadows", ["value"], [20])
      end

      for data_source <- DataSource.all() do
        assert [10] = Query.build_shadow(data_source, "shadows", "value")
      end
    end

    test "only a configured number of most popular values are kept" do
      for i <- 1..max_values() do
        :ok = insert_rows(_user_ids = 0..12, "shadows", ["value"], [i])
      end

      :ok = insert_rows(_user_ids = 0..11, "shadows", ["value"], [0])

      for data_source <- DataSource.all() do
        assert MapSet.new(Query.build_shadow(data_source, "shadows", "value")) == MapSet.new(1..max_values())
      end
    end

    test "builds an empty list for userless tables" do
      for data_source <- DataSource.all() do
        assert Query.build_shadow(data_source, "shadows_userless", "value") == []
      end
    end

    test "[BUG] building a shadow from an encoded column" do
      :ok = insert_rows(_user_ids = 0..20, "shadows", ["encoded_value"], ["123" |> Base.encode64()])

      for data_source <- DataSource.all() do
        assert ["123"] = Query.build_shadow(data_source, "shadows", "encoded_value")
      end
    end

    defp max_values(), do: Application.get_env(:cloak, :shadow_tables) |> Keyword.fetch!(:size)
  end
end

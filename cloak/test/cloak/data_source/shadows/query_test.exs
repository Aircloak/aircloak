defmodule Cloak.DataSource.Shadows.Query.Test do
  use ExUnit.Case, async: false

  import Cloak.Test.QueryHelpers
  alias Cloak.DataSource
  alias Cloak.DataSource.Shadows.Query

  setup_all do
    :ok = Cloak.Test.DB.create_table("shadows", "value INTEGER")
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

    test "only 100 most popular values are kept" do
      for i <- 1..100 do
        :ok = insert_rows(_user_ids = 0..12, "shadows", ["value"], [i])
      end

      :ok = insert_rows(_user_ids = 0..11, "shadows", ["value"], [0])

      for data_source <- DataSource.all() do
        assert MapSet.new(Query.build_shadow(data_source, "shadows", "value")) == MapSet.new(1..100)
      end
    end
  end
end

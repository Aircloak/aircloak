defmodule Cloak.DataSource.Shadows.Query.Test do
  use ExUnit.Case, async: false

  import Cloak.Test.QueryHelpers
  alias Cloak.DataSource
  alias Cloak.DataSource.Shadows.Query

  setup_all do
    :ok = Cloak.Test.DB.create_table("shadows", "value INTEGER, encoded_value TEXT")

    :ok =
      Cloak.Test.DB.create_table("shadows_encoded", nil,
        skip_db_create: true,
        query: "select user_id, value, dec_b64(encoded_value) as encoded_value from shadows"
      )

    :ok = Cloak.Test.DB.create_table("shadows_userless", "value INTEGER", user_id: nil, content_type: :public)
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
        assert ["123"] = Query.build_shadow(data_source, "shadows_encoded", "encoded_value")
      end
    end

    test "does not build shadow tables when configured not to do so" do
      :ok = insert_rows(_user_ids = 0..20, "shadows", ["value"], [10])
      :ok = insert_rows(_user_ids = 0..20, "shadows", ["value"], [20])

      for data_source <- DataSource.all() do
        data_source =
          update_in(data_source, [Lens.key(:tables) |> Lens.map_values()], &Map.put(&1, :maintain_shadow_db, false))

        assert [] = Query.build_shadow(data_source, "shadows", "value")
      end
    end

    defp max_values(), do: Application.get_env(:cloak, :shadow_tables) |> Keyword.fetch!(:size)
  end
end

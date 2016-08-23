defmodule Air.DataSourceTest do
  use Air.ModelCase

  alias Air.{Cloak, DataSource}

  test "registering data source replaces old" do
    assert nil == Repo.one(DataSource)

    data_source_data = %{
      "id" => "data_source_name",
      "tables" => [
        %{
          "columns" => [
            %{"name" => "uid", "type" => "text"}
          ]
        }
      ]
    }

    Cloak.register("test cloak", [data_source_data])

    data_source = Repo.one!(DataSource)
    assert data_source.name == "data_source_name"
    assert DataSource.tables(data_source) == data_source_data["tables"]
  end
end

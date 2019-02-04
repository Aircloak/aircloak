defmodule Air.Service.AnalystTableTest do
  # because of shared mode
  use Air.SchemaCase, async: false

  alias Air.{Service.AnalystTable, TestRepoHelper}

  setup do
    Ecto.Adapters.SQL.Sandbox.mode(Repo, {:shared, self()})
    group = TestRepoHelper.create_group!()

    u1 = TestRepoHelper.create_user!(%{groups: [group.id]})
    u2 = TestRepoHelper.create_user!(%{groups: [group.id]})

    ds1 = TestRepoHelper.create_data_source!(%{groups: [group.id]})
    ds2 = TestRepoHelper.create_data_source!(%{groups: [group.id]})

    %{u1: u1, u2: u2, ds1: ds1, ds2: ds2}
  end

  describe ".all/0" do
    test "lists all analyst tables irrespective of user and data source", context do
      create_analyst_table(context[:ds1], context[:u1], "name1")
      create_analyst_table(context[:ds2], context[:u2], "name2")

      assert ["name1", "name2"] =
               AnalystTable.all()
               |> Enum.map(& &1.name)
               |> Enum.sort()
    end
  end

  describe ".all/2" do
    test "lists an analysts table for a data source", context do
      create_analyst_table(context[:ds1], context[:u1], "name")

      assert ["name"] =
               AnalystTable.all(context[:u1], context[:ds1])
               |> Enum.map(& &1.name)
    end

    test "excludes analyst tables from other data sources", context do
      create_analyst_table(context[:ds2], context[:u1], "name")

      assert [] =
               AnalystTable.all(context[:u1], context[:ds1])
               |> Enum.map(& &1.name)
    end

    test "excludes analyst tables by other users", context do
      create_analyst_table(context[:ds1], context[:u2], "name")

      assert [] =
               AnalystTable.all(context[:u1], context[:ds1])
               |> Enum.map(& &1.name)
    end
  end

  defp create_analyst_table(data_source, user, name),
    do:
      %Air.Schemas.AnalystTable{}
      |> Ecto.Changeset.cast(
        %{
          user_id: user.id,
          data_source_id: data_source.id,
          name: name,
          sql: "sql for #{name}",
          registration_info: "Registration info"
        },
        ~w(name sql user_id data_source_id registration_info)a
      )
      |> Repo.insert!()
end

defmodule Air.Service.AnalystTableTest do
  # because of shared mode
  use Air.SchemaCase, async: false

  alias Air.Service.AnalystTable

  setup do
    Ecto.Adapters.SQL.Sandbox.mode(Repo, {:shared, self()})
    group = TestRepoHelper.create_group!()

    u1 = TestRepoHelper.create_user!(%{groups: [group.id]})
    u2 = TestRepoHelper.create_user!(%{groups: [group.id]})

    ds = TestRepoHelper.create_data_source!(%{groups: [group.id]})

    %{u1: u1, u2: u2, ds: ds}
  end

  describe ".all/0" do
    test "lists all analyst tables irrespective of user and data source", context do
      create_analyst_table(context[:ds], context[:u1], "name1")
      create_analyst_table(context[:ds], context[:u2], "name2")

      assert ["name1", "name2"] =
               AnalystTable.all()
               |> Enum.map(& &1.name)
               |> Enum.sort()
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

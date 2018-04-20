defmodule Air.Service.Export.Test do
  use Air.SchemaCase, async: true

  alias Air.Service.{AuditLog, Export}
  import Air.TestRepoHelper

  setup do
    {:ok, %{user: create_user!()}}
  end

  test "includes user information", %{user: user} do
    assert export(user)["user"]["name"] == user.name
  end

  test "includes user group information" do
    group1 = create_group!()
    group2 = create_group!()
    user = create_user!(%{groups: [group1.id, group2.id]})

    assert export(user)["user"]["groups"] == [group1.name, group2.name]
  end

  test "includes audit logs", %{user: user} do
    AuditLog.log(user, "Some audit log", %{some: "metadata"})
    AuditLog.log(user, "Some other audit log", %{some: "metadata"})

    assert [
             %{"event" => "Some audit log", "metadata" => %{"some" => "metadata"}},
             %{"event" => "Some other audit log"}
           ] = export(user)["audit_logs"]
  end

  defp export(user) do
    {:ok, result} =
      Export.reduce_while(user, [], fn next, acc ->
        {:cont, [acc, next]}
      end)

    result
    |> to_string()
    |> Poison.decode!()
  end
end

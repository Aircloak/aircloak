defmodule Air.TaskTest do
  use Air.ModelCase

  alias Air.Task

  @valid_attrs %{
    id: "7488a646-e31f-11e4-aace-600308960662", name: "some content",
    query: "some content", permanent: false
  }
  @invalid_attrs %{}

  test "has display name if no name is set" do
    assert Task.display_name(%Task{}) == "Unnamed task"
    assert Task.display_name(%Task{name: "name"}) == "name"
  end

  test "removes old temporary tasks" do
    changeset = Task.changeset(%Task{}, @valid_attrs)
    {:ok, task} = Repo.insert(changeset)
    assert {0, nil} == Task.remove_temporary_tasks

    Repo.update(gen_changeset(task, %{inserted_at: days_ago(8)}))
    assert {1, nil} == Task.remove_temporary_tasks
  end

  test "does not remove old permanent tasks" do
    changeset = Task.changeset(%Task{}, @valid_attrs)
    {:ok, task} = Repo.insert(changeset)

    Repo.update(gen_changeset(task, %{inserted_at: days_ago(8), permanent: true}))
    assert {0, nil} == Task.remove_temporary_tasks
  end

  defp days_ago(n) do
    base_date = :calendar.datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}})
    time_days_ago = :os.system_time(:seconds) + base_date - (n * 24 * 60 * 60)
    date_time = :calendar.gregorian_seconds_to_datetime(time_days_ago)
    Ecto.DateTime.from_erl(date_time)
  end

  # We hack a changeset that allows for changes that aren't otherwise
  # permissed in the Task.changeset method (like for example changing the
  # value of inserted_at).
  defp gen_changeset(task, params) do
    %Ecto.Changeset{Task.changeset(task, %{}) | changes: params}
  end
end

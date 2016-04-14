defmodule Air.TaskTest do
  use Air.ModelCase

  alias Air.Task

  @valid_attrs %{id: "7488a646-e31f-11e4-aace-600308960662", name: "some content", query: "some content"}
  @invalid_attrs %{}

  test "has display name if no name is set" do
    assert Task.display_name(%Task{}) == "Unnamed task"
    assert Task.display_name(%Task{name: "name"}) == "name"
  end
end

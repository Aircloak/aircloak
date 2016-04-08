defmodule Air.TaskTest do
  use Air.ModelCase

  alias Air.Task

  @valid_attrs %{id: "7488a646-e31f-11e4-aace-600308960662", name: "some content", query: "some content"}
  @invalid_attrs %{}

  test "changeset with valid attributes" do
    changeset = Task.changeset(%Task{}, @valid_attrs)
    assert changeset.valid?
  end

  test "changeset with invalid attributes" do
    changeset = Task.changeset(%Task{}, @invalid_attrs)
    refute changeset.valid?
  end
end

defmodule Air.ApiTokenTest do
  use ExUnit.Case, async: true
  use Air.ModelCase
  alias Air.{ApiToken}

  @valid_attrs %{description: "some content", user_id: 1}
  @invalid_attrs %{}

  test "changeset with valid attributes" do
    changeset = ApiToken.changeset(%ApiToken{}, @valid_attrs)
    assert changeset.valid?
  end

  test "changeset with invalid attributes" do
    changeset = ApiToken.changeset(%ApiToken{}, @invalid_attrs)
    refute changeset.valid?
  end
end

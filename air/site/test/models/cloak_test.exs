defmodule Air.CloakTest do
  use Air.ModelCase

  alias Air.Cloak

  @valid_attrs %{name: "some content"}
  @no_datasources []
  @invalid_attrs %{}

  test "changeset with valid attributes" do
    changeset = Cloak.changeset(%Cloak{}, @valid_attrs)
    assert changeset.valid?
  end

  test "changeset with invalid attributes" do
    changeset = Cloak.changeset(%Cloak{}, @invalid_attrs)
    refute changeset.valid?
  end

  test "default state is unknown" do
    %Cloak{}
    |> Cloak.changeset(%{name: "test cloak"})
    |> Air.Repo.insert!

    cloak = Repo.one(Cloak)
    assert Cloak.state(cloak) == :unknown
  end

  test "can set and update state" do
    %Cloak{}
    |> Cloak.changeset(%{state: :online, name: "test cloak"})
    |> Air.Repo.insert!

    cloak = Repo.one(Cloak)
    assert Cloak.state(cloak) == :online

    cloak
    |> Cloak.changeset(%{state: :offline})
    |> Air.Repo.update!

    cloak = Repo.one(Cloak)
    assert Cloak.state(cloak) == :offline
  end

  test "create or setup in one go" do
    cloak = Cloak.register("test cloak", @no_datasources)
    assert Cloak.state(cloak) == :online

    cloak
    |> Cloak.changeset(%{state: :offline})
    |> Air.Repo.update!

    cloak = Repo.one(Cloak)
    assert Cloak.state(cloak) == :offline

    # Registering it, brings it back online again
    cloak = Cloak.register("test cloak", @no_datasources)
    assert Cloak.state(cloak) == :online
  end

  test "unregistering a cloak marks it as offline" do
    Cloak.register("test cloak", @no_datasources)
    cloak = Cloak.unregister!("test cloak")
    assert Cloak.state(cloak) == :offline
  end

  test "unregistering a non-existent cloak raises an exception" do
    assert_raise RuntimeError, fn() -> Cloak.unregister!("missing cloak") end
  end

  test "registers the pid of the caller upon registration" do
    cloak = Cloak.register("test cloak", @no_datasources, self())
    assert Cloak.channel_pid(cloak) == self()
  end

  test "unregistering resets the pid" do
    Cloak.register("test cloak", @no_datasources, self())
    cloak = Cloak.unregister!("test cloak")
    refute Cloak.channel_pid(cloak) == self()
  end

  test "knows if online" do
    cloak = Cloak.register("test cloak", @no_datasources, self())
    assert Cloak.online?(cloak)
    cloak = Cloak.unregister!("test cloak")
    refute Cloak.online?(cloak)
    cloak = cloak
    |> Cloak.changeset(%{state: :unknown})
    |> Repo.update!()
    assert Cloak.state(cloak) == :unknown
    refute Cloak.online?(cloak)
  end
end

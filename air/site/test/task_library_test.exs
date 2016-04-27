defmodule Air.Socket.TaskLibraryTest do
  use ExUnit.Case, async: true

  test "no dependencies" do
    assert [] == Air.TaskLibrary.dependencies("")
  end

  test "resolving dependencies" do
    dependencies = Air.TaskLibrary.dependencies("
          Aircloak.Utils.foobar()
          Aircloak.Utils1.foobar()
          Foo.Bar.baz()
          Aircloak.FooBar.foobar()
        ")
    assert [%{name: "Aircloak"}, %{name: "Aircloak.Utils"}] = dependencies
  end
end

defmodule BOM.Validate.Test do
  use ExUnit.Case, async: true

  alias BOM.{License, Package}

  @valid_package %Package{
    name: "the name",
    realm: :elixir,
    license: %License{type: :mit, text: "the text"},
    version: "1.1.1"
  }

  test "valid package", do: assert(BOM.Validate.run(@valid_package).error == nil)

  test "package with no name",
    do: assert(BOM.Validate.run(%{@valid_package | name: nil}).error == "Name empty")

  test "package with empty name",
    do: assert(BOM.Validate.run(%{@valid_package | name: ""}).error == "Name empty")

  test "package with no license",
    do: assert(BOM.Validate.run(%{@valid_package | license: nil}).error == "No license")

  test "package with no realm",
    do: assert(BOM.Validate.run(%{@valid_package | realm: nil}).error == "Realm empty")

  test "package with no version",
    do: assert(BOM.Validate.run(%{@valid_package | version: nil}).error == "Version empty")

  test "package with empty version",
    do: assert(BOM.Validate.run(%{@valid_package | version: ""}).error == "Version empty")
end

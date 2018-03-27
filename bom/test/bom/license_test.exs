defmodule BOM.License.Test do
  use ExUnit.Case, async: true

  alias BOM.License

  test "type of a normal license", do: assert(License.name_to_type("MIT") == :mit)

  test "type of a weird cased license", do: assert(License.name_to_type("aPacHe-2.0") == :apache2)
end

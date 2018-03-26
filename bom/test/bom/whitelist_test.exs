defmodule BOM.Whitelist.Test do
  use ExUnit.Case, async: true

  alias BOM.{License, Package, Whitelist}

  test "guessing license type without text" do
    package = %Package{license: License.unknown()}
    assert Whitelist.update_license_type(package) == package
  end
end

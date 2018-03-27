defmodule AirWeb.LicenseView do
  @moduledoc false
  use Air.Web, :view

  def type_counts(packages) do
    packages
    |> group_by_type()
    |> Enum.map(fn {type, packages} -> {type, length(packages)} end)
    |> Enum.sort_by(fn {_type, count} -> count end, &>=/2)
  end

  def group_by_type(packages) do
    packages
    |> Enum.group_by(& &1.license_type)
    |> Enum.sort_by(fn {type, _packages} -> type end)
    |> Enum.map(fn {type, packages} -> {type, Enum.sort_by(packages, & &1.name)} end)
  end

  def license_name("mit"), do: "MIT License"
  def license_name("isc"), do: "ISC License"
  def license_name("bsd_2_clause"), do: "BSD 2-Clause"
  def license_name("bsd_3_clause"), do: "BSD 3-Clause"
  def license_name("bsd_4_clause"), do: "BSD 4-Clause"
  def license_name("apache2"), do: "Apache License, Version 2.0"
  def license_name("public_domain"), do: "Public Domain"
  def license_name("zlib"), do: "The zlib/libpng License"
  def license_name("boost"), do: "Boost Software License 1.0"
  def license_name("do_what_the_fuck_you_want"), do: "Do What the Fuck You Want To Public License"
  def license_name("mpl_2_0"), do: "Mozilla Public License, Version 2.0"
  def license_name(type), do: type
end

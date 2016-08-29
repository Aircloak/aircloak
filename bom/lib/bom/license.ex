defmodule BOM.License do
  @moduledoc "Utilities for manipulating licenses."

  defstruct [:type, :text]

  @type t :: %__MODULE__{type: atom, text: String.t}


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Returns a license struct with a generic text (as opposed to one provided by the package author that includes
  the list of contributors).
  """
  @spec find_by_type(atom) :: t
  def find_by_type(type) do
    %__MODULE__{type: type, text: text(type)}
  end

  @doc """
  Converts common ways a license is specified by name to the same atom. For example returns :apache2 for both
  "Apache-2.0" and "Apache version 2.0".
  """
  @spec name_to_type(String.t) :: atom
  def name_to_type("MIT"), do: :mit
  def name_to_type("MIT/X11"), do: :mit
  def name_to_type("ISC"), do: :isc
  def name_to_type("FreeBSD"), do: :bsd_3_clause
  def name_to_type("BSD"), do: :bsd_3_clause
  def name_to_type("BSD-3-Clause"), do: :bsd_3_clause
  def name_to_type("BSD-2-Clause"), do: :bsd_2_clause
  def name_to_type("Apache License, Version 2.0"), do: :apache2
  def name_to_type("Apache version 2.0"), do: :apache2
  def name_to_type("Apache-2.0"), do: :apache2
  def name_to_type("Public Domain"), do: :public_domain
  def name_to_type("Unlicense"), do: :public_domain
  def name_to_type("BOOST"), do: :boost
  def name_to_type("WTFPL"), do: :do_what_the_fuck_you_want
  def name_to_type("Zlib"), do: :zlib
  def name_to_type("zlib"), do: :zlib
  def name_to_type(_), do: :unknown

  @doc "Returns true if we can use the given license type in the product, false otherwise."
  @spec allowed_type?(atom) :: boolean
  for path <- Path.wildcard("./licenses/generic/*") do
    name = Path.basename(path) |> String.to_atom
    def allowed_type?(unquote(name)), do: true
  end
  def allowed_type?(:boost), do: true
  def allowed_type?(:do_what_the_fuck_you_want), do: true
  def allowed_type?(:zlib), do: true
  def allowed_type?(_), do: false


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  for path <- Path.wildcard("./licenses/generic/*") do
    name = Path.basename(path) |> String.to_atom
    text = File.read!(path)

    defp text(unquote(name)) do
      unquote(text)
    end
  end
end

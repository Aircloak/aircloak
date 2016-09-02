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
  def name_to_type(name), do: name |> String.downcase() |> do_name_to_type()

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

  @doc "Returns struct representing an unknown license."
  @spec unknown() :: t
  def unknown(), do: %__MODULE__{type: :unknown, text: ""}


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp do_name_to_type("mit"), do: :mit
  defp do_name_to_type("mit/x11"), do: :mit
  defp do_name_to_type("isc"), do: :isc
  defp do_name_to_type("freebsd"), do: :bsd_3_clause
  defp do_name_to_type("bsd"), do: :bsd_3_clause
  defp do_name_to_type("bsd-3-clause"), do: :bsd_3_clause
  defp do_name_to_type("bsd-2-clause"), do: :bsd_2_clause
  defp do_name_to_type("apache license, version 2.0"), do: :apache2
  defp do_name_to_type("apache version 2.0"), do: :apache2
  defp do_name_to_type("apache-2.0"), do: :apache2
  defp do_name_to_type("public domain"), do: :public_domain
  defp do_name_to_type("unlicense"), do: :public_domain
  defp do_name_to_type("boost"), do: :boost
  defp do_name_to_type("wtfpl"), do: :do_what_the_fuck_you_want
  defp do_name_to_type("zlib"), do: :zlib
  defp do_name_to_type(_), do: :unknown

  for path <- Path.wildcard("./licenses/generic/*") do
    name = Path.basename(path) |> String.to_atom
    text = File.read!(path)

    defp text(unquote(name)) do
      unquote(text)
    end
  end
end

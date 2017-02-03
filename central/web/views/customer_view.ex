defmodule Central.CustomerView do
  @moduledoc false
  use Central.Web, :view
  # bug in the current Phoenix
  @dialyzer :no_match

  defp ensure_non_empty_list([], default_el), do: [default_el]
  defp ensure_non_empty_list([_|_] = list, _default_el), do: list
end

defmodule Air.Ecto.Pg do
  @moduledoc """
  Ecto does a good job of providing wrappers for general SQL syntax.
  However, Postgres provides a *lot* of useful features in writing queries,
  that it would be sad not to make use of them.
  """
  require Ecto.Query

  @doc "Models the WITHIN GROUP statement used to run functions on ordered subsets."
  defmacro within_group(agg, [{:order_by, order}]),
    do: quote(do: fragment("? WITHIN GROUP (ORDER BY ?)", unquote(agg), unquote(order)))

  @doc "Computes the specified percentile, performing linear interpolation if necessary."
  defmacro percentile_cont(p) do
    quote do
      fragment("percentile_cont(?)", unquote(p))
    end
  end

  @doc "Computes the specified percentile, or the nearest value."
  defmacro percentile_disc(p) do
    quote do
      fragment("percentile_disc(?)", unquote(p))
    end
  end

  @doc "Computes buckets on the source"
  defmacro width_bucket(field, min, max, bins),
    do: quote(do: fragment("width_bucket(?, ?, ?, ?)", unquote(field), unquote(min), unquote(max), unquote(bins)))

  @doc "Access a field in a JSONB or JSON field. Note: the return type is JSON(b) so, you may need to wrap in `type/2`."
  defmacro a ~> b do
    quote do
      fragment("(? -> ?)", unquote(a), unquote(b))
    end
  end
end

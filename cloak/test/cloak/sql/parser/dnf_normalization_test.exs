defmodule Cloak.Sql.Parser.DNFNormalization.Test do
  use ExUnit.Case, async: true
  use ExUnitProperties
  import StreamData

  alias Cloak.Sql.Parser.DNFNormalization

  describe ".dnf" do
    property "evaluates the same" do
      check all(
              terms <- uniq_list_of(atom(:alphanumeric), length: 10),
              assignment <- assignment(terms),
              formula <- formula(terms)
            ) do
        assert eval(DNFNormalization.dnf(formula), assignment) == eval(formula, assignment)
      end
    end

    property "is in disjunctive normal form" do
      check all(
              terms <- uniq_list_of(atom(:alphanumeric), length: 10),
              formula <- formula(terms)
            ) do
        assert dnf?(DNFNormalization.dnf(formula))
      end
    end

    property "does nothing when acting on a conjunction" do
      check all(
              terms <- uniq_list_of(atom(:alphanumeric), length: 10),
              formula <- conjunction(terms)
            ) do
        assert DNFNormalization.dnf(formula) == formula
      end
    end
  end

  defp dnf?(formula, top_level? \\ true)
  defp dnf?(a, _) when is_atom(a), do: true
  defp dnf?({:not, a}, _), do: is_atom(a)
  defp dnf?({:or, a, b}, true), do: dnf?(a, true) and dnf?(b, true)
  defp dnf?({:or, _a, _b}, false), do: false
  defp dnf?({:and, a, b}, _), do: dnf?(a, false) and dnf?(b, false)

  defp eval({:or, a, b}, assignment), do: eval(a, assignment) or eval(b, assignment)
  defp eval({:and, a, b}, assignment), do: eval(a, assignment) and eval(b, assignment)
  defp eval({:not, term}, assignment), do: not eval(term, assignment)
  defp eval(term, assignment), do: Map.fetch!(assignment, term)

  defp assignment(terms) do
    list_of(boolean(), length: length(terms))
    |> map(fn bools -> terms |> Enum.zip(bools) |> Enum.into(%{}) end)
  end

  defp conjunction(terms, depth \\ 10) do
    frequency([{10, :term}, {depth, :and}])
    |> bind(fn
      :term -> one_of([{:not, one_of(terms)}, one_of(terms)])
      :and -> {:and, conjunction(terms, depth - 1), conjunction(terms, depth - 1)}
    end)
  end

  defp formula(terms, depth \\ 10) do
    frequency([{10, :term}, {depth, :not}, {depth, :and}, {depth, :or}])
    |> bind(fn
      :term -> one_of(terms)
      :not -> {:not, formula(terms, depth - 1)}
      :and -> {:and, formula(terms, depth - 1), formula(terms, depth - 1)}
      :or -> {:or, formula(terms, depth - 1), formula(terms, depth - 1)}
    end)
  end
end

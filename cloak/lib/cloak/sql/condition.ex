defmodule Cloak.Sql.Condition do
  @moduledoc "Contains utility functions for working with conditions."

  alias Cloak.Sql.{Query, Expression}

  @inequalities ~w(< > <= >=)
  @comparisons ~w(= <>) ++ @inequalities

  @doc "Return true if the given where clause is an inequality (<, >, <=, >=), false otherwise."
  @spec inequality?(Query.where_clause()) :: boolean
  def inequality?(%Expression{kind: :function, name: "not", args: [arg]}), do: inequality?(arg)
  def inequality?(%Expression{kind: :function, name: name}) when name in @inequalities, do: true
  def inequality?(_), do: false

  @doc "Returns true if the given where clause is an equality (=), false otherwise."
  @spec equals?(Query.where_clause()) :: boolean
  def equals?(%Expression{kind: :function, name: "="}), do: true
  def equals?(_), do: false

  @doc "Returns true if the given where clause is a not-equals clause (<>), false otherwise."
  @spec not_equals?(Query.where_clause()) :: boolean
  def not_equals?(%Expression{kind: :function, name: "<>"}), do: true
  def not_equals?(_), do: false

  @doc "Returns true if the given where clause is a LIKE clause, false otherwise."
  @spec like?(Query.where_clause()) :: boolean
  def like?(%Expression{kind: :function, name: name}) when name in ~w(like ilike), do: true
  def like?(_), do: false

  @doc "Returns true if the given where clause is a NOT (I)LIKE clause, false otherwise."
  @spec not_like?(Query.where_clause()) :: boolean
  def not_like?(%Expression{kind: :function, name: "not", args: [arg]}), do: like?(arg)
  def not_like?(_), do: false

  @doc "Returns true if the given where clause is a NOT ILIKE clause, false otherwise."
  @spec not_ilike?(Query.where_clause()) :: boolean
  def not_ilike?(%Expression{kind: :function, name: "not", args: [%Expression{kind: :function, name: "ilike"}]}),
    do: true

  def not_ilike?(_), do: false

  @doc "Returns true if the given where clause is an IN clause, false otherwise."
  @spec in?(Query.where_clause()) :: boolean
  def in?(%Expression{kind: :function, name: "in"}), do: true
  def in?(_), do: false

  @doc "Returns the targets of the condition."
  @spec targets(Query.where_clause()) :: [Expression.t()]
  def targets(%Expression{kind: :function, name: "not", args: [arg]}), do: targets(arg)
  def targets(%Expression{kind: :function, args: args}), do: args

  @doc "Returns the subject term the given condition acts on."
  @spec subject(Query.where_clause()) :: Expression.t()
  def subject(condition), do: condition |> targets() |> Enum.at(0)

  @doc "Returns the constant term the given condition compares against."
  @spec value(Query.where_clause()) :: any
  def value(condition) do
    condition
    |> targets()
    |> case do
      [_subject] -> nil
      [_subject, value] -> Expression.const_value(value)
      [_subject | values] -> Enum.map(values, &Expression.value/1)
    end
  end

  @doc "Returns a representation of the direction of the given inequality as `:<` or `:>`."
  @spec direction(Query.where_clause()) :: :> | :<
  def direction(%Expression{kind: :function, name: name}) when name in ~w(< <=), do: :<
  def direction(%Expression{kind: :function, name: name}) when name in ~w(> >=), do: :>

  @doc "Converts the given condition to a function that checks a row."
  @spec to_function(Query.where_clause()) :: (any -> boolean) | nil
  def to_function(nil), do: nil

  def to_function(condition), do: &Expression.value(condition, &1)

  @doc "Returns the verb of the condition."
  @spec verb(Query.where_clause()) :: :comparison | :is_null | :in | :like
  def verb(%Expression{kind: :function, name: "not", args: [arg]}), do: verb(arg)
  def verb(%Expression{kind: :function, name: "is_null"}), do: :is_null
  def verb(%Expression{kind: :function, name: "in"}), do: :in
  def verb(%Expression{kind: :function, name: name}) when name in @comparisons, do: :comparison
  def verb(%Expression{kind: :function, name: name}) when name in ~w(like ilike), do: :like

  @doc "Combines two conditions with the AND operator."
  @spec both(Query.where_clause(), Query.where_clause()) :: Query.where_clause()
  def both(nil, nil), do: nil
  def both(lhs, nil), do: lhs
  def both(nil, rhs), do: rhs
  def both(lhs, rhs), do: Expression.function("and", [lhs, rhs], :boolean)

  @doc "Rejects conditions that match the given function."
  @spec reject(Query.where_clause(), (Query.where_clause() -> boolean)) :: Query.where_clause()
  def reject(nil, _matcher), do: nil

  def reject(%Expression{kind: :function, name: name, args: [lhs, rhs]} = expression, matcher)
      when name in ~w(or and) do
    case {reject(lhs, matcher), reject(rhs, matcher)} do
      {nil, nil} -> nil
      {nil, rhs} -> rhs
      {lhs, nil} -> lhs
      {lhs, rhs} -> %Expression{expression | args: [lhs, rhs]}
    end
  end

  def reject(condition, matcher), do: if(matcher.(condition), do: nil, else: condition)

  @doc "Splits the conditions tree into matchning and non-matching trees."
  @spec partition(Query.where_clause(), (Query.where_clause() -> boolean)) ::
          {Query.where_clause(), Query.where_clause()}
  def partition(conditions, matcher) do
    non_matching = reject(conditions, matcher)
    matching = reject(conditions, &(not matcher.(&1)))
    {matching, non_matching}
  end
end

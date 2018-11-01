defmodule Air.Service.Query.State do
  @moduledoc "Helper functions for working with query states."

  running_states = [
    :started,
    :parsing,
    :compiling,
    :awaiting_data,
    :ingesting_data,
    :processing,
    :post_processing
  ]

  active_states = [:created | running_states]

  completed_states = [
    :start_expired,
    :cancelled,
    :error,
    :completed
  ]

  all_states = active_states ++ completed_states

  combinations = fn
    [], _combinator -> []
    [head | tail], combinations -> Enum.map(tail, &{head, &1}) ++ combinations.(tail, combinations)
  end

  valid_state_transitions =
    [{:created, :started}] ++
      combinations.(running_states, combinations) ++
      for active_state <- active_states, completed_state <- completed_states, do: {active_state, completed_state}

  union_type = fn values ->
    values = Enum.reverse(values)
    Enum.reduce(tl(values), hd(values), &quote(do: unquote(&1) | unquote(&2)))
  end

  @type t :: unquote(union_type.(all_states))
  @type active :: unquote(union_type.(active_states))
  @type completed :: unquote(union_type.(completed_states))

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns the list of all known query states."
  @spec all() :: [t]
  def all(), do: unquote(all_states)

  @doc "Returns the list of all states representing an active (not finished) query."
  @spec active() :: [active()]
  def active(), do: unquote(active_states)

  @doc "Returns the list of all states representing a complete query."
  @spec completed() :: [completed()]
  def completed(), do: unquote(completed_states)

  @doc "Returns true if the state transition from the given state to the given state is valid."
  @spec valid_state_transition?(t, t) :: boolean
  def valid_state_transition?(from_state, to_state)

  def valid_state_transition?(state, state), do: true

  Enum.each(
    valid_state_transitions,
    fn {from_state, to_state} -> def valid_state_transition?(unquote(from_state), unquote(to_state)), do: true end
  )

  def valid_state_transition?(_from_state, _to_state), do: false
end

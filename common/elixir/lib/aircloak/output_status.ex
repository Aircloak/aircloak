defmodule Aircloak.OutputStatus do
  @moduledoc """
  Utility module for outputting  status style messages ala:

    Foo bar       [OK]
    Foo bar       [PENDING]
    Foo bar       [ERROR]

  Usage:

    Aircloak.OutputStatus.new_line("Foo bar", :pending)
    Aircloak.OutputStatus.update_state("Foo bar", :pending, "Custom state")
    Aircloak.OutputStatus.done("Foo bar")
  """

  @description_pad 80
  # Should allow for the longest status too, to clear the line
  @total_pad 120

  @type state :: :done | :pending | :warning | :error

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @spec new_line(String.t(), state, String.t() | nil) :: :ok
  @doc """
  Creates a new status line that can subsequently be updated.
  The description is used in the status field, and the state determines the colour.

  The available states are:
  - done: green
  - pending: blue
  - warning: yellow
  - error: red

  Called as `new_line("Text", :pending, "Description")` the result would be:

    Text       [Description]
  """
  def new_line(text, state, description \\ nil) when is_atom(state),
    do:
      text
      |> String.pad_trailing(@description_pad)
      |> append_state(description || state, state_colour(state))
      |> String.pad_trailing(@total_pad)
      |> IO.write()

  @spec update_state(String.t(), state, String.t() | nil) :: :ok
  @doc """
  Allows changing the state of a previously started status line.
  The text should be identical for good results, but the description could change,
  and likely so would the state.

  If it previously read:

    Testing       [querying]

  then calling `update_state("Testing", :pending, "takes time")` would yield the line

    Testing       [takes time]

  with the colour blue from the fact that the :pending state was used.
  """
  def update_state(text, state, description \\ nil) do
    IO.write("\r")
    new_line(text, state, description)
  end

  @spec done(String.t()) :: :ok
  @doc """
  Finalizes a status line as being successful.
  If it previously read:

    Testing       [pending]

  it becomes

    Testing       [done]
  """
  def done(text), do: end_line(text, :done)

  @spec end_line(String.t(), state) :: :ok
  @doc """
  Finalizes a status line with a custom status type.
  Useful if the operation described failed.
  If it previously read:

    Testing       [querying]

  it could become

    Testing       [error]

  if called with status `:error`

  Note the status state determines the colour of the state description.
  """
  def end_line(text, status) do
    update_state(text, status)
    IO.puts("")
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp append_state(text, state, colour), do: "#{text}#{colour}[#{state}]#{IO.ANSI.reset()}"

  defp state_colour(:done), do: IO.ANSI.green()
  defp state_colour(:pending), do: IO.ANSI.blue()
  defp state_colour(:warning), do: IO.ANSI.yellow()
  defp state_colour(:error), do: IO.ANSI.red()
end

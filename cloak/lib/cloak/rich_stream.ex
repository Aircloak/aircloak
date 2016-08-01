defmodule Cloak.RichStream do
  @moduledoc """
  A stream which can transform an input into another input.

  This stream is a different take on `Stream.resource`. It has following
  properties:

  - statefulness: a client can change the state of the stream on each input element
  - optional emitting: for each input value, a client can emit zero or more output values
  - post-process emitting: after the input is processed, client can emit additional values based on the
    stream state

  See `new/4` for more details.
  """

  defstruct [:input, :state, :handle_fun, :after_fun, :reducer_acc, :reducer_fun]

  @type state :: any
  @type element :: any
  @type input_element :: any
  @type output_value :: any
  @type handle_fun :: ((state, input_element) -> {[output_value], state})
  @type after_fun :: ((state) -> [output_value])


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Creates a new rich lazy stream.

  Arguments:

  - `input` - input enumerable which is consumed
  - `state` - initial stream state
  - `handle_fun` - invoked on each input element. A function must return a list of output values
    as well as the next stream state
  - `after_fun` - invoked after input is exhausted to produce additional values.
  """
  @spec new(Enumerable.t, state, handle_fun, after_fun) :: Enumerable.t
  def new(input, state, handle_fun, after_fun) do
    %__MODULE__{input: input, state: state, handle_fun: handle_fun, after_fun: after_fun}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defimpl Enumerable do
    @doc false
    alias Cloak.RichStream

    def count(_), do: {:error, __MODULE__}

    def member?(_, _), do: {:error, __MODULE__}

    def reduce(stream, {:cont, reducer_acc}, reducer_fun) do
      stream = %RichStream{stream | reducer_acc: reducer_acc, reducer_fun: reducer_fun}
      case Enumerable.reduce(stream.input, {:cont, stream}, &reducer/2) do
        {:done, stream} ->
          post_process(stream)

        {:halted, stream} ->
          {:halted, stream.reducer_acc}
      end
    end

    defp reducer(input_element, stream) do
      {output_values, state} = stream.handle_fun.(stream.state, input_element)
      Enum.reduce(
        output_values,
        {:cont, %RichStream{stream | state: state}},
        fn(output_value, {_previous_action, stream}) ->
          {reducer_action, reducer_acc} = stream.reducer_fun.(output_value, stream.reducer_acc)
          {reducer_action, %RichStream{stream | reducer_acc: reducer_acc}}
        end
      )
    end

    defp post_process(stream) do
      Enumerable.reduce(stream.after_fun.(stream.state), {:cont, stream.reducer_acc}, stream.reducer_fun)
    end
  end
end

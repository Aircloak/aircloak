defmodule Cloak.Stream do
  @moduledoc """
  Additional extensions to Elixir streams.
  """

  @type acc :: any
  @type element :: any
  @type input_element :: any
  @type output_value :: any
  @type handle_fun :: ((input_element, acc) -> {Enumerable.t, acc})
  @type after_fun :: ((acc) -> Enumerable.t)


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Stateful lazy transformation of an enumerable, with the ability to produce additional
  entries after the input is exhausted.

  This function works like standard Elixir [Stream.transform/3]
  (http://elixir-lang.org/docs/stable/elixir/Stream.html#transform/3) with an
  addition that `after_fun` can produce additional output values.
  """
  @spec transform(Enumerable.t, acc, handle_fun, after_fun) :: Enumerable.t
  defdelegate transform(input, acc, handle_fun, after_fun),
    to: Cloak.Stream.TransformEnumerable,
    as: :new


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defmodule TransformEnumerable do
    @moduledoc false

    defstruct [:input, :acc, :handle_fun, :after_fun, :consumer_acc, :consumer_fun]

    def new(input, acc, handle_fun, after_fun), do:
      %__MODULE__{input: input, acc: acc, handle_fun: handle_fun, after_fun: after_fun}

    defimpl Enumerable do
      @moduledoc false

      def count(_), do: {:error, __MODULE__}

      def member?(_, _), do: {:error, __MODULE__}

      def reduce(stream, {:cont, consumer_acc}, consumer_fun) do
        stream = %TransformEnumerable{stream | consumer_acc: consumer_acc, consumer_fun: consumer_fun}
        case Enumerable.reduce(stream.input, {:cont, stream}, &input_reducer/2) do
          {:done, stream} ->
            post_process(stream)

          {:halted, stream} ->
            {:halted, stream.consumer_acc}
        end
      end

      defp input_reducer(input_element, stream) do
        {output_values, acc} = stream.handle_fun.(input_element, stream.acc)

        Enum.reduce(
          output_values,
          {:cont, %TransformEnumerable{stream | acc: acc}},
          fn
            (_, {:halt, _} = acc) -> acc
            (output_value, {:cont, stream}) ->
              {reducer_action, consumer_acc} = stream.consumer_fun.(output_value, stream.consumer_acc)
              {reducer_action, %TransformEnumerable{stream | consumer_acc: consumer_acc}}
          end
        )
      end

      defp post_process(stream) do
        Enumerable.reduce(
          stream.after_fun.(stream.acc),
          {:cont, stream.consumer_acc},
          stream.consumer_fun
        )
      end
    end
  end
end

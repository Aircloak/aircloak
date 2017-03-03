defmodule Air.Error do
  @moduledoc "Utilities for working with errors."

  require Logger

  @doc """
  Runs fun catching exception. If an exception is thrown logs it and returns {:error, :internal_error}.
  Returns result of fun otherwise.
  """
  @spec exception_to_tuple((() -> x)) :: x when x: var
  def exception_to_tuple(fun) do
    try do
      fun.()
    catch type, error ->
      Logger.error([
        "Error encountered #{inspect(type)} : #{inspect(error)}\n",
        Exception.format_stacktrace(System.stacktrace())
      ])

      {:error, :internal_error}
    end
  end
end

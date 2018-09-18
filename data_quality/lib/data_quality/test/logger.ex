defmodule DataQuality.Test.Logger do
  @moduledoc "Utility to produce log output during test"

  @padding 2

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @spec banner(String.t()) :: :ok
  @doc """
  Outputs a banner to the screen, along the lines of:

    #########
    # MAGIC #
    #########

  """
  def banner(text) do
    top_line = repeat("#", String.length(text) + 2 * @padding)
    IO.puts("\n\n" <> top_line <> "\n# #{text} #\n" <> top_line <> "\n")
  end

  @spec header(String.t()) :: :ok
  @doc "Outputs a log line formatted as a header to screen"
  def header(text) do
    text = text <> ":"
    IO.puts("\n#{text}")
    IO.puts(repeat("=", String.length(text)))
  end

  @spec log(String.t()) :: :ok
  @doc "Outputs a log line to screen"
  def log(text), do: IO.puts(text)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp repeat(what, times),
    do:
      what
      |> List.duplicate(times)
      |> Enum.join()
end

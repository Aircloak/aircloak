defmodule DataQuality.Distributions.Beta do
  @moduledoc """
  Utility for generating beta distribution values, that then subsequently
  get further adapted such that they are spread out as desired.

  Relies on R to generate the data.
  """
  require Logger

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @spec generate(integer, integer, integer, float, float) :: [float]
  @doc "Generates a list of beta-values scaled to be in the range min-max."
  def generate(min, max, num_samples, alpha, beta) do
    port = Port.open({:spawn, "Rscript beta.r #{num_samples} #{alpha} #{beta}"}, [:binary])
    await_data(port, min, max - min, [])
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp await_data(port, min, range, acc) do
    receive do
      {^port, {:data, result}} ->
        values =
          result
          |> String.split()
          |> Enum.map(fn
            "0" -> 0.0
            "1" -> 1.0
            other -> String.to_float(other)
          end)
          |> Enum.map(fn val ->
            val * range + min
          end)

        await_data(port, min, range, values ++ acc)

      other ->
        Logger.error("Unexpected result from R: #{inspect(other)}")
        []
    after
      500 ->
        acc
    end
  end
end

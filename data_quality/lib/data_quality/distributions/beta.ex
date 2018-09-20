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

  @spec generate(integer, integer, integer, float, float) :: Enumerable.t()
  @doc "Generates a list of beta-values scaled to be in the range min-max."
  def generate(min, max, num_samples, alpha, beta),
    do:
      Stream.resource(
        fn -> Port.open({:spawn, "Rscript beta.r #{num_samples} #{alpha} #{beta}"}, [:binary]) end,
        &await_data(&1, min, max - min),
        fn _port -> :ok end
      )

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp await_data(port, min, range) do
    receive do
      {^port, {:data, result}} ->
        values =
          result
          |> String.split()
          |> Enum.map(&elem(Float.parse(&1), 0))
          |> Enum.map(fn val -> val * range + min end)

        {values, port}

      _other ->
        {:halt, port}
    after
      500 -> {:halt, port}
    end
  end
end

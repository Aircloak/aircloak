defmodule DataQuality.Distributions.Beta.Test do
  use ExUnit.Case

  alias DataQuality.Distributions.Beta

  describe ".generate" do
    test "generates N samples" do
      num_samples = 100
      samples = gen(num_samples: num_samples)

      assert Enum.count(samples) == num_samples
    end

    test "takes min and max values into account" do
      min = 100
      max = 200
      samples = gen(min: min, max: max, num_samples: 1000)

      assert Enum.min(samples) >= min
      assert Enum.max(samples) <= max
    end

    test "returns floating point values", do: assert(Enum.all?(gen(), &is_float/1))
  end

  defp gen(params \\ []) do
    defaults = [
      min: 0.0,
      max: 1.0,
      num_samples: 100,
      alpha: 1,
      beta: 1
    ]

    params = Keyword.merge(defaults, params)
    Beta.generate(params[:min], params[:max], params[:num_samples], params[:alpha], params[:beta])
  end
end

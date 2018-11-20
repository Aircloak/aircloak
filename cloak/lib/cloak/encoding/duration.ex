defimpl Jason.Encoder, for: Timex.Duration do
  def encode(duration, options), do: duration |> Timex.Duration.to_string() |> Jason.Encode.string(options)
end

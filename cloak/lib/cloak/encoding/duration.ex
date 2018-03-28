defimpl Poison.Encoder, for: Timex.Duration do
  def encode(duration, options), do: duration |> Timex.Duration.to_string() |> Poison.Encoder.BitString.encode(options)
end

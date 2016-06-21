defimpl Poison.Encoder, for: Timex.DateTime do
  def encode(datetime, options) do
    datetime
    |> Timex.format!("{ISOz}")
    |> Poison.Encoder.BitString.encode(options)
  end
end

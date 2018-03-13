defimpl Inspect, for: ExPublicKey.RSAPrivateKey do
  @impl Inspect
  def inspect(_, _), do: "%ExPublicKey.RSAPrivateKey{}"
end

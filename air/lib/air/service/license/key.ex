defmodule Air.Service.License.Key do
  @moduledoc """
  Contains the public keys used to verify licenses. Provides a tiny bit of security from tampering by making the keys be
  part of the compiled binary.
  """

  if Mix.env() == :prod do
    @prod_key """
    -----BEGIN PUBLIC KEY-----
    MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAokg2mD1vsqTjSYz30Ss9
    YqZJP8jddWTw8SQZpFDXxtcf0BCFqle+WDAAS4qVrJ+J+yiZ/QwJIgZDqv0jgO7m
    +PNW3h4i0Sw0gS6D8jHJF74y7yy0a+1eK/DFwQ0ZBR8/XfM71aOa8xTHs8IIzM/A
    u1Va1Oa+0RPBbHwU80dnGH4N6G+A+JD0GXjiJqfdtKqKQd4t804Iax1IJ7nfMPFx
    9Iqg8fLT3n71Z4ZSO9/KVMU6DoAquc0nztWsA5+Apa5HRajYeF1XH/ynmRvT2pdy
    Dvvn+N3x9GbqB2zXslj5wODiddnJCwlYc2hh6wUJJULsh7pVyLFsBt7TbNqczzH+
    WwIDAQAB
    -----END PUBLIC KEY-----
    """

    @stage_key """
    -----BEGIN PUBLIC KEY-----
    MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAxJMm0dBfeb+gj+1bGm/+
    AkN+9lvgHH4blHVJoq2YTKCnJ99+Bc9w7ysMA41ijWMpDPBeMWYJEh/AXpnrE3vi
    y9RtCIKka20HMXQH7Wp5Pj0eT3fAJ+JW8WcqMneAcTTYzAo3B5d3DduQiI4Psqg7
    7WrEGZynzrL/8qXPtPch7/Lz/N07R8lj6+Ye+3EmF8y3GIP1UbmvCfJ/ABjGdk3m
    3IhaSpE2+wd3xi3OmnqIVbgbvbtJ0uefkydcDj4IyRwecdPGIX5ZdUiM0ixrWIcb
    TZBGyx/aebd11xf+3uqWYbo5dPm9k8fVeP6wGRTDOlzEPE15eohxHSi5Nv80d36p
    RQIDAQAB
    -----END PUBLIC KEY-----
    """
  else
    @dev_key """
    -----BEGIN PUBLIC KEY-----
    MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAt/AhcEKYHReOQRsghFnC
    BSVjdGwI8SNrFqDJ2t1LyH2JhnWfBzkyfAZmnZIxO2SxRc+UKkm9iwxUOUQGj4dG
    ajeorIFEVHbBJEXcIvv1b9Q6MM5nZ8LjUKSucAfGeoscgEA/tDt4NA2UYngi8+ds
    61h/TmbBPdlRrh6JurFImmTMcwzNtRdJZ4P79B4pzmBHd1yRy2vR0EtKnFw0+nFX
    K2WwmAelrSCPZjUwrKEE2DuJxupaOI2L3+FvE5NU7eI9Y83URPZ2MypJnb9rXbv4
    /qKOuna5L2fl7P+1f4cjFX8IQrWn5dnra8LD/T5HbtRy67v+wMYetP1m7hpscSzc
    4wIDAQAB
    -----END PUBLIC KEY-----
    """
  end

  @doc "Returns the public key that should be used for license verification."
  @spec public_key() :: ExPublicKey.RSAPublicKey.t
  if Mix.env() == :prod do
    def public_key() do
      require Aircloak.DeployConfig

      case Map.fetch(Aircloak.DeployConfig.fetch!("site"), "use_staging_license_key") do
        {:ok, true} -> ExPublicKey.loads!(@stage_key)
        _ -> ExPublicKey.loads!(@prod_key)
      end
    end
  else
    def public_key(), do: ExPublicKey.loads!(@dev_key)
  end
end

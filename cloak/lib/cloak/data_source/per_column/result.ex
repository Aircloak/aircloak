defmodule Cloak.DataSource.PerColumn.Result do
  @moduledoc "Holds a shareable analysis result."

  alias Cloak.DataSource.PerColumn.Descriptor
  require Aircloak.DeployConfig

  @type source :: Cloak.DataSource.Bounds.Cache | Cloak.DataSource.Isolators.Cache | Cloak.DataSource.Shadows.Cache
  @type t :: %__MODULE__{
          descriptor: Descriptor.t(),
          status: :ok,
          expires: NaiveDateTime.t(),
          result: any,
          type: source
        }
  defstruct [:descriptor, :status, :expires, :result, :type]

  @mode :aes_gcm
  @aad "AES256GCM"

  @doc "Constructs a new result"
  @spec new(Descriptor.t(), source(), any, NaiveDateTime.t()) :: t
  def new(descriptor, type, result, expires) do
    %__MODULE__{
      descriptor: descriptor,
      status: :ok,
      expires: expires,
      result: result,
      type: type
    }
  end

  @doc "Decrypts an encrypted Result struct"
  @spec decrypt(%{descriptor: Descriptor.t(), status: :ok, expires: NaiveDateTime.t(), result: any, type: source}) ::
          t
  def decrypt(result_data) do
    %__MODULE__{
      descriptor: result_data.descriptor,
      status: result_data.status,
      expires: result_data.expires,
      result: decrypt_result(result_data.result),
      type: result_data.type
    }
  end

  @doc "Encrypts a Result struct for transmission to other cloaks"
  @spec encrypt(t()) :: t
  def encrypt(d) do
    %__MODULE__{d | result: encrypt_result(d.result)}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp decrypt_result(<<iv::binary-16, tag::binary-16, ciphertext::binary>>) do
    :erlang.binary_to_term(:crypto.block_decrypt(@mode, secret_key!(), iv, {@aad, ciphertext, tag}))
  end

  defp encrypt_result(result) do
    iv = :crypto.strong_rand_bytes(16)

    {ciphertext, ciphertag} =
      :crypto.block_encrypt(@mode, secret_key!(), iv, {@aad, :erlang.term_to_binary(result), 16})

    iv <> ciphertag <> ciphertext
  end

  defp secret_key!() do
    key = Aircloak.DeployConfig.fetch!("encryption_key")
    unless byte_size(key) == 16, do: raise("Encryption key must be 16 bytes!")
    key
  end
end

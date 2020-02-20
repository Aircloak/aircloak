defmodule Cloak.DataSource.PerColumn.Result do
  @moduledoc """
  Holds a shareable analysis result.
  """
  alias Cloak.DataSource.PerColumn.Descriptor

  @type t :: %{
          descriptor: Descriptor.t(),
          status: :ok,
          expires: NaiveDateTime.t(),
          result: any,
          type: Cloak.DataSource.Bounds.Cache | Cloak.DataSource.Isolators.Cache | Cloak.DataSource.Shadows.Cache
        }
  defstruct [:descriptor, :status, :expires, :result, :type]

  def new(descriptor, type, result, expires) do
    %__MODULE__{
      descriptor: descriptor,
      status: :ok,
      expires: expires,
      result: result,
      type: type
    }
  end

  def decrypt(d) do
    %__MODULE__{
      descriptor: d.descriptor,
      status: d.status,
      expires: d.expires,
      result: decrypt_result(d.result),
      type: d.type
    }
  end

  def encrypt(d) do
    %__MODULE__{d | result: encrypt_result(d.result)}
  end

  defp decrypt_result(%{encrypted: result}) do
    result
  end

  defp encrypt_result(result) do
    %{encrypted: result}
  end
end

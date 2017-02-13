defmodule Central.Schemas.OnlineStatus do
  @moduledoc "Custom type for online status."
  @behaviour Ecto.Type

  @type t :: :offline | :online


  # -------------------------------------------------------------------
  # Ecto.Type behaviour
  # -------------------------------------------------------------------

  @doc false
  def type(), do: :integer

  @doc false
  def cast(:offline), do: {:ok, :offline}
  def cast(:online), do: {:ok, :online}
  def cast(_), do: :error

  @doc false
  def load(0), do: {:ok, :offline}
  def load(1), do: {:ok, :online}
  def load(_), do: :error

  @doc false
  def dump(:offline), do: {:ok, 0}
  def dump(:online), do: {:ok, 1}
  def dump(_), do: :error
end

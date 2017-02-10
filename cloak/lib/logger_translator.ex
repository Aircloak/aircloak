defmodule Cloak.LoggerTranslator do
  @moduledoc """
  Custom logger translator which ensures that sensitive data is not logged on process crashes.
  """

  ## ----------------------------------------------------------------
  ## API functions
  ## ----------------------------------------------------------------

  @doc "Installs this module as the logger translator."
  @spec install() :: :ok
  def install(), do:
    Logger.add_translator({__MODULE__, :translate})


  ## ----------------------------------------------------------------
  ## Logger translator callbacks
  ## ----------------------------------------------------------------

  @doc false
  def translate(min_level, level, kind, message), do:
    Logger.Translator.translate(min_level, level, kind, message)
end

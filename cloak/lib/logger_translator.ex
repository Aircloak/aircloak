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
  def translate(min_level, :error, :format, message) do
    with {:ok, filtered_message} <- filter_error_message(message), do:
      forward_translate(min_level, :error, :format, filtered_message)
  end
  def translate(min_level, level, kind, message), do:
    forward_translate(min_level, level, kind, message)


  ## ----------------------------------------------------------------
  ## Internal functions
  ## ----------------------------------------------------------------

  defp forward_translate(min_level, level, kind, message), do:
    Logger.Translator.translate(min_level, level, kind, message)

  defp filter_error_message({'** Generic server ' ++ _ = msg, [name, _last, _state, reason]}), do:
    {:ok, {msg, [name, "filtered", "filtered", filter_reason(reason)]}}
  defp filter_error_message({'** Task ' ++ _ = msg, [name, starter, function, args, reason]}), do:
    {:ok, {msg, [name, starter, function, Enum.map(args, fn(_) -> "filtered" end), filter_reason(reason)]}}
  defp filter_error_message(_), do: :skip

  defp filter_reason({_exception, stacktrace}), do:
    {"filtered",
      stacktrace
      |> Enum.map(fn
        {_mod, _fun, arity, _location} = entry when is_integer(arity) -> entry
        {mod, fun, args, location} when is_list(args) -> {mod, fun, length(args), location}
        _other -> nil
      end)
      |> Enum.filter(&(&1 != nil))
    }
end

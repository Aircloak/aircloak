defmodule Aircloak.ErlangLogger do
  @moduledoc """
  Helper for logging through Elixir's logger from the Erlang code.

  This module is not meant to be used directly. Instead, you should include
  `elixir_logger.hrl` and use `?DEBUG`, `?INFO`, `?WARN`, `?ERROR` macros.
  """
  require Logger

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  # Generate all macros and functions at compile time
  for log_level <- [:debug, :info, :warn, :error] do
    @doc "Logs with the #{log_level} level"
    def unquote(log_level)(format, params \\ [], meta \\ []) do
      Logger.unquote(log_level)(fn -> :io_lib.format(format, params) end, meta)
    end
  end
end

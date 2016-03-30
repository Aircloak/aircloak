defmodule Cloak.Logger do
  @moduledoc """
  Utilities for logging within the cloak application.

  This module acts as a wrapper for log operations. It extends `Logger` with
  following features:

  - Additional log levels (notice, alert, critical)
  - Anonymized logging in production
  - Wrapper functions for logging from Erlang code
  - Support for development-only logging

  To use it in the elixir code, simply add `use Cloak.Logger` at the top of your
  module. Then you can use autoimported `log_*` macros from this module. The macros
  usage is analogous to that of the `Logger` application. See
  [the Logger documentation](http://elixir-lang.org/docs/stable/logger/Logger.html)
  for more information.

  Example:

  ```
  log_info(fn -> "Log message" end)
  ```

  To log from Erlang, you can use macros defined in `cloak_logging.hrl`.

  ## Anonymized log entries

  To prevent leaking sensitive information, all log entries are in production
  replaced with generic message that outputs only the log level.

  However, even this might sometimes lead to data leakage. If you don't want
  a log entry to appear in production, you can use development only logging macros.
  In Elixir code, you can use `dev_log_*`, and in Erlang `?DEV_*` (defined in
  `cloak_logging.hrl`). These macros will produce log entries in development and
  test mode, but nothing will be printed in production.
  """
  require Logger

  @all_log_levels Enum.map([:debug, :info, :warn, :error], &{&1, &1}) ++ [
    notice: :info,
    alert: :warn,
    critical: :error
  ]


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  # Generate all macros and functions at compile time
  for {real_log_level, logger_log_level} <- @all_log_levels do
    # log_* macro for logging from the Elixir code
    macro_name = :"log_#{real_log_level}"
    @doc "Logs with the #{real_log_level} level."
    defmacro unquote(macro_name)(chardata_or_fn, metadata \\ []) do
      real_log_level = unquote(real_log_level)
      logger_log_level = unquote(logger_log_level)
      metadata = metadata ++ [log_level: real_log_level, file_name: __CALLER__.file, line_no: __CALLER__.line]

      quote do
        Logger.unquote(logger_log_level)(unquote(chardata_or_fn), unquote(metadata))
      end
    end

    # dev_log_* macro for development-only logging
    @doc """
    Logs in development and test mode with the #{real_log_level} level.

    In production mode this results in a noop, i.e. no entry is produced.
    """
    dev_macro_name = :"dev_log_#{real_log_level}"
    if Mix.env == :prod do
      defmacro unquote(dev_macro_name)(_chardata_or_fn, _metadata \\ []), do: :ok
    else
      defmacro unquote(dev_macro_name)(chardata_or_fn, metadata \\ []) do
        macro_name = unquote(macro_name)
        quote do
          unquote(macro_name)(unquote(chardata_or_fn), unquote(metadata))
        end
      end
    end

    # wrapper function for logging from Erlang
    erlang_log_fun_name = :"erlang_log_#{real_log_level}"
    @doc false
    def unquote(erlang_log_fun_name)(file_name, line_no, format, params \\ "") do
      Logger.unquote(logger_log_level)(
            fn -> :io_lib.format(format, params) end,
            log_level: unquote(real_log_level),
            file_name: file_name,
            line_no: line_no
          )
    end

    # wrapper function for development-only logging from Erlang
    erlang_dev_log_fun_name = :"erlang_dev_log_#{real_log_level}"
    @doc false
    if Mix.env == :prod do
      def unquote(erlang_dev_log_fun_name)(_file_name, _line_no, _format, _params \\ ""), do: :ok
    else
      def unquote(erlang_dev_log_fun_name)(file_name, line_no, format, params \\ "") do
        unquote(erlang_log_fun_name)(file_name, line_no, format, params)
      end
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defmacro __using__(_opts) do
    functions_to_import =
      @all_log_levels
      |> Keyword.keys
      |> Enum.flat_map(fn(level) ->
            ["log_#{level}": 1, "log_#{level}": 2, "dev_log_#{level}": 1, "dev_log_#{level}": 2]
          end)

    quote do
      require Logger
      import unquote(__MODULE__), only: unquote(functions_to_import)
    end
  end
end

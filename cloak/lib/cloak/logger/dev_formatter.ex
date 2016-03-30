if Mix.env != :prod do
  defmodule Cloak.Logger.DevFormatter do
    @moduledoc false

    # Non-production logger formatter -> we log messages, including truncated file name and line number
    @log_pattern Logger.Formatter.compile("$time [$level] $message\n")

    def format(level, msg, ts, metadata) do
      full_message = [
        case metadata[:file_name] || metadata[:file] do
          nil -> []
          file_name ->
            line = metadata[:line_no] || metadata[:line]
            [Path.basename(file_name), ?:, Integer.to_string(line), ?\s]
        end,
        msg
      ]
      Logger.Formatter.format(@log_pattern, metadata[:log_level] || level, full_message, ts, metadata)
    end
  end
end

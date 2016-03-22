defmodule Cloak.Logger.ProdFormatter do
  @moduledoc false

  # Production log formatter -> we suppress real messages and log only the log level
  # The exception is if :real_message field is passed, in which case we log that
  # message. This is used from the report handler to log crashes.
  @log_pattern Logger.Formatter.compile("$time $message\n")

  def format(level, _msg, ts, metadata) do
    real_level = metadata[:log_level] || level
    real_message = metadata[:real_message] || "The system produced #{real_level} message"
    Logger.Formatter.format(@log_pattern, level, real_message, ts, metadata)
  end
end

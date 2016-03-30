defmodule Cloak.Logger.ReportHandler do
  @moduledoc """
  Cloak report handler.

  This module acts as the production only report handler. If a crash report is
  generated, it will log the module and function where the crash occurred.
  """
  use GenEvent
  require Logger


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Installs error logger report handler (production only).
  """
  @spec install() :: :ok
  if Mix.env == :prod do
    def install do
      :error_logger.add_report_handler(Cloak.Logger.ReportHandler)
      :ok
    end
  else
    def install, do: :ok
  end


  # -------------------------------------------------------------------
  # GenEvent callbacks
  # -------------------------------------------------------------------

  @doc false
  def init(_), do: {:ok, nil}

  @doc false
  def handle_event({:error_report, _group_leader, {_pid, :crash_report, crash_data}}, state) do
    # crash report -> make a log entry and forward to logger
    [[{_reason, {module, function, parameters}}|_]|_] = crash_data
    real_message = "The system produced a crash report in #{module}.#{function}/#{length(parameters)}"
    Logger.error("", real_message: real_message)
    {:ok, state}
  end
  def handle_event(_other, state),
    do: {:ok, state}
end

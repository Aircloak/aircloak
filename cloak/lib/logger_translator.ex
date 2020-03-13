defmodule Cloak.LoggerTranslator do
  @moduledoc """
  Custom logger translator which ensures that sensitive data is not logged on process crashes.

  This translator is installed in front of standard Logger translator. In production it will blacklist all
  logs, except for the ones which are explicitly whitelisted here. The whitelisted logs are sanitized to
  remove the privacy sensitive data. This amounts to removing an exception reason, process state, or arguments
  in the function call. Consequently, crash reports will contain only stack traces of functions with arity.

  Note that this is coupled with the global configuration setting `config :sasl, :sasl_error_logger, false`,
  which should ensure that all crashes are logged only if allowed by this translator.
  """

  require Aircloak.DeployConfig

  # -------------------------------------------------------------------
  ## API functions
  # -------------------------------------------------------------------

  @doc "Installs this module as the logger translator."
  @spec install() :: :ok
  def install(), do: Logger.add_translator({__MODULE__, :translate})

  @doc "Removes sensitive information from the stacktrace."
  @spec filtered_stacktrace(any) :: any
  def filtered_stacktrace(stacktrace) do
    if Aircloak.DeployConfig.override_app_env!(:cloak, :sanitize_otp_errors) do
      do_filter_stacktrace(stacktrace)
    else
      stacktrace
    end
  end

  @doc "Formats the given exit reason."
  @spec format_exit(any) :: String.t()
  def format_exit(reason) do
    if Aircloak.DeployConfig.override_app_env!(:cloak, :sanitize_otp_errors) do
      filtered_format_exit(reason)
    else
      Exception.format_exit(reason)
    end
  end

  # -------------------------------------------------------------------
  ## Logger translator callbacks
  # -------------------------------------------------------------------

  @doc false
  def translate(min_level, level, kind, message) do
    if Aircloak.DeployConfig.override_app_env!(:cloak, :sanitize_otp_errors) do
      sanitize(min_level, level, kind, message)
    else
      # dev/test: just forward to logger
      :none
    end
  end

  # -------------------------------------------------------------------
  ## Internal functions
  # -------------------------------------------------------------------

  defp sanitize(min_level, level, kind, message) do
    case filter_message(level, kind, message) do
      {:ok, filtered_message} -> Logger.Translator.translate(min_level, level, kind, filtered_message)
      _ -> {:ok, "sanitized `#{level}`:`#{kind}` log entry"}
    end
  catch
    type, _reason ->
      {:ok, translation_error(level, kind, type, __STACKTRACE__)}
  end

  defp translation_error(level, kind, error_type, error_stacktrace),
    do: [
      "Error `#{error_type}` translating `#{level}`:`#{kind}`: ",
      Exception.format_stacktrace(filtered_stacktrace(error_stacktrace))
    ]

  defp filter_message(:error, :report, {report_type, %{} = report}) do
    {
      :ok,
      {
        report_type,
        %{report | reason: filter_reason(report.reason), state: "filtered state", last_message: "filtered message"}
      }
    }
  end

  defp filter_message(:error, :format, message), do: filter_error_message(message)
  defp filter_message(_level, _kind, _message), do: :skip

  defp filter_error_message({'** Task ' ++ _ = msg, [name, starter, function, args, reason]}),
    do: {:ok, {msg, [name, starter, function, Enum.map(args, fn _ -> "filtered" end), filter_reason(reason)]}}

  defp filter_error_message({'Error in process ' ++ _ = msg, [pid, reason]}),
    do: {:ok, {msg, [pid, filter_reason(reason)]}}

  defp filter_error_message(_), do: :skip

  defp filter_reason({:EXIT, {reason, stacktrace}}), do: {do_filter_reason(reason), do_filter_stacktrace(stacktrace)}
  defp filter_reason({exception, stacktrace}), do: {do_filter_reason(exception), do_filter_stacktrace(stacktrace)}

  defp do_filter_stacktrace(stacktrace) when is_list(stacktrace) do
    stacktrace
    |> Enum.map(fn
      {_mod, _fun, arity, _location} = entry when is_integer(arity) -> entry
      {mod, fun, args, location} when is_list(args) -> {mod, fun, length(args), location}
      _other -> nil
    end)
    |> Enum.filter(&(&1 != nil))
  end

  defp do_filter_stacktrace(_), do: []

  defp filtered_format_exit({exit_reason, stacktrace}) when is_list(stacktrace),
    do: Exception.format_exit({do_filter_reason(exit_reason), do_filter_stacktrace(stacktrace)})

  defp filtered_format_exit(_other), do: Exception.format_exit({"filtered exit reason", Aircloak.current_stacktrace()})

  # We're not filtering `Cloak.Query.ExecutionError` exception, because these errors are considered safe, so keeping
  # them in the log, helps troubleshooting.
  defp do_filter_reason(%Cloak.Query.ExecutionError{} = reason), do: reason
  defp do_filter_reason(_other), do: "filtered"
end

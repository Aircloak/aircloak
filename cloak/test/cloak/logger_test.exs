defmodule Cloak.LoggerTest do
  use ExUnit.Case, async: false
  import ExUnit.CaptureLog
  use Cloak.Logger

  setup do
    current_level = Logger.level()
    Logger.configure(level: :debug)
    on_exit(fn -> :ok = Logger.configure(level: current_level) end)
    :ok
  end

  for level <- [:debug, :info, :notice, :warn, :alert, :error, :critical] do
    macro_name = :"log_#{level}"
    test "#{macro_name}" do
      # 1-arity fun
      expected_message = "[#{unquote(level)}] #{Path.basename(__ENV__.file)}:#{__ENV__.line + 1} log_msg"
      assert(capture_log(fn -> unquote(macro_name)("log_msg") end) =~ expected_message)

      # 2-arity fun
      expected_message = "[#{unquote(level)}] #{Path.basename(__ENV__.file)}:#{__ENV__.line + 1} log_msg"
      assert(capture_log(fn -> unquote(macro_name)("log_msg", []) end) =~ expected_message)
    end

    macro_name = :"dev_log_#{level}"
    test "#{macro_name}" do
      # 1-arity fun
      expected_message = "[#{unquote(level)}] #{Path.basename(__ENV__.file)}:#{__ENV__.line + 1} log_msg"
      assert(capture_log(fn -> unquote(macro_name)("log_msg") end) =~ expected_message)

      # 2-arity fun
      expected_message = "[#{unquote(level)}] #{Path.basename(__ENV__.file)}:#{__ENV__.line + 1} log_msg"
      assert(capture_log(fn -> unquote(macro_name)("log_msg", []) end) =~ expected_message)
    end

    macro_name = :"log_#{level}"
    test "#{macro_name} production formatter" do
      console_setting = Application.get_env(:logger, :console, [])
      Logger.configure_backend(:console,
          Keyword.put(console_setting, :format, {Cloak.Logger.ProdFormatter, :format}))
      try do
        # 1-arity fun
        expected_message = "The system produced #{unquote(level)} message"
        assert(capture_log(fn -> unquote(macro_name)("log_msg") end) =~ expected_message)
        assert(capture_log(fn -> unquote(macro_name)("log_msg", []) end) =~ expected_message)
      after
        Logger.configure_backend(:console, console_setting)
      end
    end
  end
end

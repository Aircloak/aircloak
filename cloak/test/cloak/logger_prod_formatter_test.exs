defmodule Cloak.Logger.ProdFormatterTest do
  use ExUnit.Case, async: false
  import ExUnit.CaptureLog
  require Logger

  setup do
    current_level = Logger.level()
    console_setting = Application.get_env(:logger, :console, [])

    Logger.configure_backend(:console,
        Keyword.put(console_setting, :format, {Cloak.Logger.ProdFormatter, :format}))
    Logger.configure(level: :debug)

    on_exit(fn ->
          :ok = Logger.configure(level: current_level)
          Logger.configure_backend(:console, console_setting)
        end)
    :ok
  end

  for level <- [:debug, :info, :warn, :error] do
    test "#{level} production formatter" do
      expected_message = "The system produced #{unquote(level)} message"
      assert(capture_log(fn -> Logger.unquote(level)("log_msg") end) =~ expected_message)
      assert(capture_log(fn -> Logger.unquote(level)("log_msg", []) end) =~ expected_message)
    end
  end
end

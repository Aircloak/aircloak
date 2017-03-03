defmodule Air.Error.Test do
  use ExUnit.Case, async: false

  alias Air.Error
  import ExUnit.CaptureLog

  describe "exception_to_tuple" do
    test "success", do:
      assert :result = Error.exception_to_tuple(fn() -> :result end)

    test "exception" do
      capture_log(fn() ->
        assert {:error, :internal_error} = Error.exception_to_tuple(fn() -> raise "error cause" end)
      end)
    end

    test "logging" do
      assert capture_log(fn() ->
        Error.exception_to_tuple(fn() -> raise "error cause" end)
      end) =~ "error cause"
    end
  end
end

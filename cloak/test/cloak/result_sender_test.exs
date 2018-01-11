defmodule Cloak.ResultSender.Test do
  use ExUnit.Case, async: true

  alias Cloak.ResultSender

  test "preserves execution_time" do
    ResultSender.send_result({:process_encoded, self()}, %{execution_time: 100})
    assert_receive %{execution_time: 100}
  end
end

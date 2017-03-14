defmodule Air.Schemas.CentralCallTest do
  use ExUnit.Case, async: true
  alias Air.Schemas.CentralCall

  test "export is consistent" do
    call = CentralCall.new("event", %{"some" => "payload"})
    assert CentralCall.export(call) == CentralCall.export(call)
  end

  test "different calls have different exported ids" do
    call1 = CentralCall.new("event", %{"some" => "payload"})
    call2 = CentralCall.new("event", %{"some" => "payload"})
    e1 = CentralCall.export(call1)
    e2 = CentralCall.export(call2)
    assert e1.id != e2.id
  end
end

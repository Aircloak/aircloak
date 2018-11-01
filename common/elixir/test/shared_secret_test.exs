defmodule Aircloak.SharedSecret.Test do
  use ExUnit.Case, async: true

  alias Aircloak.SharedSecret

  test "proof generated with a given key can be verified with the same key" do
    assert :ok = SharedSecret.proof("some key") |> SharedSecret.verify("some key")
  end

  test "proof generated with a given key cannot be verified with another key" do
    assert :error = SharedSecret.proof("some key") |> SharedSecret.verify("another key")
  end
end

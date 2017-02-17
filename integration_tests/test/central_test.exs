defmodule IntegrationTest.CentralTest do
  use ExUnit.Case, async: false

  alias Central.Repo
  import Ecto.Query

  test "air status is stored in database" do
    assert air_status() == 1

    Supervisor.terminate_child(Air.Supervisor, Air.CentralSocket)
    :timer.sleep(100)
    assert air_status() == 0

    Supervisor.restart_child(Air.Supervisor, Air.CentralSocket)
    :timer.sleep(100)
    assert air_status() == 1
  end

  defp air_status() do
    Repo.one!(from(
      a in "airs",
      join: c in Central.Schemas.Customer, on: a.customer_id == c.id,
      where: c.name == "integration tests customer",
      select: a.status
    ))
  end
end

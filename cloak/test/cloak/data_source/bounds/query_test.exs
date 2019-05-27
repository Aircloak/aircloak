defmodule Cloak.DataSource.Bounds.Query.Test do
  use ExUnit.Case, async: false

  alias Cloak.DataSource
  alias Cloak.DataSource.Isolators.Query

  setup_all do
    :ok = Cloak.Test.DB.create_table("bounds", "value INTEGER, pk INTEGER")

    :ok =
      Cloak.Test.DB.create_table(
        "bounds with spaces",
        "\"user id\" INTEGER, \"val ue\" INTEGER",
        user_id: "user id"
      )
  end

  setup do
    :ok = Cloak.Test.DB.clear_table("bounds")
    :ok = Cloak.Test.DB.clear_table("bounds with spaces")

    anonymizer_config = Application.get_env(:cloak, :anonymizer)
    Application.put_env(:cloak, :anonymizer, anonymizer_config |> Keyword.put(:bound_threshold, 10))
    on_exit(fn -> Application.put_env(:cloak, :anonymizer, anonymizer_config) end)
  end

  test "users are uniformly distributed"

  test ""

  test "non-numeric columns have unknown bounds"

  test "names with spaces are handled properly"
end

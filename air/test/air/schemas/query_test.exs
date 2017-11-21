defmodule Air.Schemas.QueryTest do
  use ExUnit.Case, async: false
  import Air.TestRepoHelper
  alias Air.Schemas.Query

  setup do
    Ecto.Adapters.SQL.Sandbox.checkout(Air.Repo)
    Ecto.Adapters.SQL.Sandbox.mode(Air.Repo, {:shared, self()})
    :ok
  end

  test "for_display of a finished query", do:
    Enum.each(
      [:error, :completed, :cancelled],
      &(assert %{completed: true} = display(%{query_state: &1}))
    )


  test "for_display of an unfinished query", do:
    Enum.each(
      [:created, :started, :parsing, :compiling, :awaiting_data, :ingesting_data, :processing, :post_processing],
      &(assert %{completed: false} = display(%{query_state: &1}))
    )

  test "for_display includes all data from result", do:
    assert %{"some" => "data"} = display(%{result: %{"some" => "data"}})

  test "for_display when result is not preloaded" do
    query = create_user!() |> create_query!(%{result: %{rows: [], columns: []}})
    query_without_result = Air.Repo.get!(Query, query.id)
    display = Query.for_display(query_without_result)

    assert :error == Map.fetch(display, :rows)
    assert :error == Map.fetch(display, :columns)
  end

  defp display(create_query_params), do:
    create_user!()
    |> create_query!(create_query_params)
    |> Query.for_display()
end

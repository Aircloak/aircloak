defmodule AirWeb.QueryTest do
  use ExUnit.Case, async: false
  import Air.TestRepoHelper

  setup do
    Ecto.Adapters.SQL.Sandbox.checkout(Air.Repo)
    Ecto.Adapters.SQL.Sandbox.mode(Air.Repo, {:shared, self()})
    :ok
  end

  test "for_display of a finished query" do
    Enum.each(
      [:error, :completed, :cancelled],
      &assert(%{completed: true} = display(%{query_state: &1}))
    )
  end

  test "for_display of an unfinished query" do
    Enum.each(
      [
        :created,
        :started,
        :parsing,
        :compiling,
        :awaiting_data,
        :ingesting_data,
        :processing,
        :post_processing
      ],
      &assert(%{completed: false} = display(%{query_state: &1}))
    )
  end

  test "for_display includes all data from result",
    do: assert(%{"some" => "data"} = display(%{result: %{"some" => "data"}}))

  test "for_display when result is not preloaded" do
    query = create_user!() |> create_query!(%{result: %{rows: [], columns: []}})
    query_without_result = Air.Repo.get!(Air.Schemas.Query, query.id)
    display = AirWeb.Query.for_display(query_without_result)

    assert :error == Map.fetch(display, :rows)
    assert :error == Map.fetch(display, :columns)
  end

  test "includes the owner's name" do
    user = create_user!()
    assert AirWeb.Query.for_display(create_query!(user)).user.name == user.name
  end

  test "includes the data source name" do
    data_source = create_data_source!()
    assert display(%{data_source_id: data_source.id}).data_source.name == data_source.name
  end

  defp display(create_query_params),
    do:
      create_user!()
      |> create_query!(create_query_params)
      |> AirWeb.Query.for_display()
end

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
      ~w(created started parsing compiling awaiting_data ingesting_data processing post_processing)a,
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

  test "provided links when public permalink is used" do
    query = create_query!(create_user!())
    token = Air.Service.Token.public_query_token(query)
    display = AirWeb.Query.for_display(query, authenticated?: false, permalink_token: token)

    assert String.starts_with?(display.buckets_link, "/permalink/public")
    assert is_nil(display.public_permalink)
    assert is_nil(display.private_permalink)
  end

  test "provided links when private permalink is used" do
    query = create_query!(create_user!())
    token = Air.Service.Token.private_query_token(query)
    display = AirWeb.Query.for_display(query, authenticated?: false, permalink_token: token)

    assert String.starts_with?(display.buckets_link, "/permalink/private")
    assert is_nil(display.public_permalink)
    assert is_nil(display.private_permalink)
  end

  test "provided links when authenticated" do
    query = create_query!(create_user!())
    display = AirWeb.Query.for_display(query, authenticated?: true)

    assert display.buckets_link == "/queries/#{query.id}/buckets"
    assert String.starts_with?(display.public_permalink, "/permalink/public/query/")
    assert String.starts_with?(display.private_permalink, "/permalink/private/query/")
  end

  test "for display is by default non-authenticated" do
    query = create_query!(create_user!())
    assert AirWeb.Query.for_display(query) == AirWeb.Query.for_display(query, authenticated?: false)
  end

  defp display(create_query_params, for_display_opts \\ []),
    do:
      create_user!()
      |> create_query!(create_query_params)
      |> AirWeb.Query.for_display(for_display_opts)
end

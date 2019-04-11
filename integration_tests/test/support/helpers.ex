defmodule IntegrationTest.Helpers do
  def run_query(user, query, params \\ []) do
    data_source_id_spec = {:id, IntegrationTest.Manager.data_source().id}
    {:ok, query} = Air.Service.Query.create(data_source_id_spec, :autogenerate, user, :http, query, params, [])
    Air.Service.DataSource.await_query(query)
  end

  def unique_name(scope), do: "#{scope}_#{:erlang.unique_integer([:positive])}"
end

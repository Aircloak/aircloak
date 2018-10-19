defmodule Air.Service.Salts do
  @known_names ~w[api_token password_reset]a
  @salt_size 64

  alias Air.Repo
  alias Air.Schemas.Salt
  import Ecto.Query

  def get(server \\ __MODULE__, name) do
    unless name in @known_names do
      raise "Unknown salt"
    end

    Agent.get_and_update(server, &do_get(&1, name))
  end

  defp do_get(cache, name) do
    Map.get_and_update(cache, name, fn
      nil ->
        salt = fetch_from_db(name)
        {salt, salt}

      salt ->
        {salt, salt}
    end)
  end

  defp fetch_from_db(name) do
    Salt
    |> where([q], q.name == ^to_string(name))
    |> Repo.one()
    |> case do
      nil -> create_salt!(name).value
      salt -> salt.value
    end
  end

  defp create_salt!(name) do
    %Salt{}
    |> Ecto.Changeset.change(%{
      name: to_string(name),
      value: :crypto.strong_rand_bytes(@salt_size) |> Base.encode64()
    })
    |> Repo.insert!()
  end

  def child_spec(_arg), do: Aircloak.ChildSpec.agent(fn -> %{} end, name: __MODULE__)
end

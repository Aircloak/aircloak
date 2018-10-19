defmodule Air.Service.Salts do
  @known_names ~w[api_token password_reset session_signing session_encryption]a
  @salt_size 64

  alias Air.Repo
  alias Air.Schemas.Salt
  import Ecto.Query
  import Aircloak, only: [in_env: 1]

  def get(server \\ __MODULE__, name) do
    unless name in @known_names do
      raise "Unknown salt"
    end

    in_env(test: get_test(server, name), else: do_get(server, name))
  end

  defp get_test(__MODULE__, name), do: to_string(name)
  defp get_test(server, name), do: do_get(server, name)

  defp do_get(server, name) do
    Agent.get_and_update(server, &fetch_from_cache(&1, name))
  end

  defp fetch_from_cache(cache, name) do
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
    |> Ecto.Changeset.change(%{name: to_string(name), value: random_string()})
    |> Repo.insert!()
  end

  defp random_string(), do: :crypto.strong_rand_bytes(@salt_size) |> Base.encode64()

  def child_spec(_arg), do: Aircloak.ChildSpec.agent(fn -> %{} end, name: __MODULE__)
end

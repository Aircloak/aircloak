defmodule Air.Service.Salts do
  @moduledoc """
  This module manages lazily-generated salt values to be used for various cryptographic needs. The values are stored in
  the database and cached in-memory. Thanks to generating these in a deployed instance we avoid keeping the values in
  our codebase and/or requiring additional configuration options. It also makes it extremely easy to add a new salt.
  """

  @known_names ~w[api_token password_reset session_signing session_encryption]a
  @salt_size 64

  alias Air.Repo
  alias Air.Schemas.Salt
  import Ecto.Query
  import Aircloak, only: [in_env: 1]

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Returns the named salt. The name is checked against a list of registered names. This is done in order to avoid typos
  in matching calls such as `Phoenix.Token.sign`/`Phoenix.Token.verify`.
  """
  @spec get(Agent.agent(), atom()) :: String.t()
  def get(server \\ __MODULE__, name) do
    unless name in @known_names do
      raise "Unknown salt"
    end

    in_env(test: get_test(server, name), else: do_get(server, name))
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

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

  if Mix.env() == :test do
    defp get_test(__MODULE__, name), do: to_string(name)
    defp get_test(server, name), do: do_get(server, name)
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  def child_spec(_arg), do: Aircloak.ChildSpec.agent(fn -> %{} end, name: __MODULE__)
end

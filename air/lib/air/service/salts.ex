defmodule Air.Service.Salts do
  @moduledoc """
  This module manages generated salt values to be used for various cryptographic needs. The values are stored in the
  database and cached in-memory. Thanks to generating these in a deployed instance we avoid keeping the values in our
  codebase and/or requiring additional configuration options. It also makes it extremely easy to add a new salt.
  """

  @known_names ~w[api_token password_reset session_signing session_encryption]a
  @salt_size 64

  alias Air.Repo
  alias Air.Schemas.Salt
  import Ecto.Query

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Returns the named salt. The name is checked against a list of registered names. This is done in order to avoid typos
  in matching calls such as `Phoenix.Token.sign`/`Phoenix.Token.verify`.
  """
  @spec get(atom()) :: String.t()
  def get(name) do
    case Application.get_env(:air, __MODULE__) |> Map.fetch(name) do
      :error -> raise "Unknown salt"
      {:ok, result} -> result
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  def setup_salts() do
    salts =
      @known_names
      |> Enum.map(&{&1, fetch_from_db(&1)})
      |> Enum.into(%{})

    Application.put_env(:air, __MODULE__, salts)

    :proc_lib.init_ack({:ok, self()})
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

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  def child_spec(_arg) do
    %{
      id: __MODULE__,
      # using :proc_lib ensures that the supervisor will start the next child only after the salts have been setup
      start: {:proc_lib, :start_link, [__MODULE__, :setup_salts, []]},
      restart: :transient
    }
  end
end

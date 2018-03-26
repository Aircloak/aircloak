defmodule Air.Utils do
  @moduledoc "Various utility functions."

  @doc """
  Updates the application environment.

  Invokes the `updater_fn` lambda with the existing value. The lambda must
  return the new value which is then stored to the application environment using
  the provided `put_env_opts`
  (see [Application.put_env/4](http://elixir-lang.org/docs/stable/elixir/Application.html#put_env/4)
  for description).
  """
  @spec update_app_env(
          Application.app(),
          Application.key(),
          [timeout: timeout, persistent: boolean],
          (Application.value() -> Application.value())
        ) :: :ok
  def update_app_env(app, key, put_env_opts \\ [], updater_fn),
    do: Application.put_env(app, key, updater_fn.(Application.get_env(app, key)), put_env_opts)
end

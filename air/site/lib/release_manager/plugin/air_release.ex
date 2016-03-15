defmodule ReleaseManager.Plugin.AirRelease do
  @moduledoc false
  use ReleaseManager.Plugin

  # ReleaseManager.Plugin behaviour is not in PLT since exrm is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  def before_release(_config), do: :ok

  def after_release(%Config{} = config) do
    # We'll place our own `rel/boot_script` in place of rel/bin/air/air.
    # This will allow us to set some default environment variables.
    :ok = File.rename("rel/#{config.name}/bin/#{config.name}", "rel/#{config.name}/bin/release")
    File.cp!("rel/boot_script", "rel/#{config.name}/bin/#{config.name}")
  end

  def after_package(_config), do: :ok

  def after_cleanup(_args), do: :ok
end

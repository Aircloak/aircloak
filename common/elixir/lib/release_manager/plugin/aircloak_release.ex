defmodule ReleaseManager.Plugin.AircloakRelease do
  @moduledoc """
  Plugin which support custom boot_script.

  This plugin allows placing custom `boot_script` file which can be used
  to configure relx environment substitutions. In your project, you need to
  create `rel/boot_script` file (it needs to have executable permissions).

  Here's an example:

  ```
  #!/bin/sh

  # Set default variables
  export RELX_REPLACE_OS_VARS=true
  export CLOAK_NAME=${CLOAK_NAME:-"cloak"}

  # call the control script
  $(dirname "$0")/release $@
  ```

  Now, in your `vm.args`, you can use environment substitutions:

  ```
  -name ${CLOAK_NAME}@127.0.0.1
  ...
  ```

  With such configuration in place, you can now set `CLOAK_NAME` environment
  variable to specify the VM name.
  """
  use ReleaseManager.Plugin

  # ReleaseManager.Plugin behaviour is not in PLT since exrm is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  @doc false
  def before_release(_config), do: :ok

  @doc false
  def after_release(%Config{} = config) do
    # We'll place our own `rel/boot_script` in place of rel/`app`/bin/`app`.
    # This will allow us to set some default environment variables.
    :ok = File.rename("rel/#{config.name}/bin/#{config.name}", "rel/#{config.name}/bin/release")
    File.cp!("rel/boot_script", "rel/#{config.name}/bin/#{config.name}")
  end

  @doc false
  def after_package(_config), do: :ok

  @doc false
  def after_cleanup(_args), do: :ok
end

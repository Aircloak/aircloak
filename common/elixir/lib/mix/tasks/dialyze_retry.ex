defmodule Mix.Tasks.DialyzeRetry do
  use Mix.Task
  @shortdoc "Runs dialyzer on the project with retry logic"
  @moduledoc """
  Mix task for running dialyzer with retry logic.

  Dialyze mix task caches PLT in the _build folder. This is usually good as it
  speeds up dialyzing process significantly. Unfortunately there is a problem if
  some module from some dependency has been deleted. PLT check will fail in this
  case, as it can't remove the missing module.

  The error can be manually solved by deleting the PLT file and retrying.
  However, this is quite cumbersome, especially in CI builds, where one has
  to find and delete the correct cache folder. This hacky wrapper will detect
  such error and try to fix it by removing the PLT file and retrying PLT check
  once more.
  """

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  @recursive true

  @impl Mix.Task
  def run(args) do
    try do
      Mix.Task.run("dialyze", ["--no-analyse"])
    rescue Mix.Error ->
      # possible stale cache -> delete plts and retry once more
      for plt_file <- Path.wildcard("#{Mix.Project.build_path()}/*.plt"),
        do: File.rm!(plt_file)

      Mix.Task.reenable("dialyze")
      Mix.Task.run("dialyze", ["--no-analyse"])
    end

    # Finally, run dialyzer without checking PLT (this has been done above)
    Mix.Task.reenable("dialyze")
    Mix.Task.run("dialyze", ["--no-check" | args])
  end
end

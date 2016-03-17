defmodule Mix.Tasks.Compile.Sandbox do
  @shortdoc "Compiles the lua sandbox"
  @moduledoc "Compiles the lua sandbox"
  use Mix.Task

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  @doc false
  def run(_args) do
    system_cmd!("make", ["-C", "lua_sandbox", "depend"])
    system_cmd!("make", ["-C", "lua_sandbox"])
    File.mkdir_p!("priv")
    File.cp!("lua_sandbox/obj/lua_sandbox", "priv/sandbox")
    File.cp!("lua_sandbox/src/helpers.lua", "priv/helpers.lua")
    File.cp!("lua_sandbox/src/serialization.lua", "priv/serialization.lua")
  end

  defp system_cmd!(cmd, args) do
    case System.cmd(cmd, args, stderr_to_stdout: true) do
      {_, 0} -> :ok
      {output, _} ->
        IO.puts "\n#{IO.ANSI.red()}#{output}#{IO.ANSI.reset()}"
        Mix.raise("Error running #{cmd}")
    end
  end
end

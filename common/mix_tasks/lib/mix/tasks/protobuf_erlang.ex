defmodule Mix.Tasks.Compile.Protobuf.Erlang do
  @shortdoc "Erlang compilation of protobuf files in the `proto` folder"
  @moduledoc """
  Erlang compilation of protobuf files in the `proto` folder

  For all files in the `proto` folder (and subfolders), this task will generate
  `include/*_pb.hrl` and `ebin/*_pb.beam` files.
  """

  use Mix.Task

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  @doc false
  def run(_args) do
    Logger.configure(level: :warn)
    config = Mix.Project.config()

    for proto_src <- Path.wildcard("proto/**/*.proto"),
        compile_path = Mix.Project.compile_path(config),
        stale?(proto_src, compile_path)
    do
      :protobuffs_compile.scan_file(to_char_list(proto_src),
            output_include_dir: 'include',
            output_ebin_dir: to_char_list(compile_path)
          )
      Mix.shell.info("Compiled #{proto_src}")
    end

    :ok
  end

  defp stale?(proto_src, ebin_folder) do
    Mix.Utils.stale?([proto_src], [Path.join("include", pb_file(proto_src, "hrl"))]) or
    Mix.Utils.stale?([proto_src], [Path.join(ebin_folder, pb_file(proto_src, "beam"))])
  end

  defp pb_file(proto_src, ext), do: "#{Path.basename(proto_src, ".proto")}_pb.#{ext}"
end

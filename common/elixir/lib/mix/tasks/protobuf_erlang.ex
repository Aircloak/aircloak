defmodule Mix.Tasks.Compile.Protobuf.Erlang do
  @shortdoc "Erlang compilation of protobuf files in the `proto` folder"
  @moduledoc """
  Erlang compilation of protobuf files in the `proto` folder

  For all files in the `proto` folder (and subfolders), this task will generate
  `include/*_pb.hrl` and `ebin/*_pb.beam` files.

  Usage: Simply add `:"protobuf.erlang"` to the list of compilers in `mix.exs`. For example:

  ```elixir
  def project do
    [
      compilers: [:"protobuf.erlang", :yecc, :leex, :erlang, :elixir, :app],
      # ...
    ]
  end
  ```
  """

  use Mix.Task

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  @doc false
  def run(_args) do
    old_logger_level = Logger.level
    try do
      Logger.configure(level: :warn)
      config = Mix.Project.config()

      for proto_src <- Path.wildcard("proto/**/*.proto"),
          compile_path = Mix.Project.compile_path(config),
          stale?(proto_src, compile_path)
      do
        :protobuffs_compile.scan_file(to_char_list(proto_src),
              output_include_dir: 'include',
              output_ebin_dir: to_char_list(compile_path),
              compile_flags: config[:erlc_options]
            )

        Path.join(compile_path, pb_file(proto_src, "beam"))
        |> to_char_list
        |> add_dialyze_attribute(config[:erlc_options])

        Mix.shell.info("Compiled #{proto_src}")
      end

      :ok
    after
      Logger.configure(level: old_logger_level)
    end
  end

  defp stale?(proto_src, ebin_folder) do
    Mix.Utils.stale?([proto_src], [Path.join("include", pb_file(proto_src, "hrl"))]) or
    Mix.Utils.stale?([proto_src], [Path.join(ebin_folder, pb_file(proto_src, "beam"))])
  end

  defp pb_file(proto_src, ext), do: "#{Path.basename(proto_src, ".proto")}_pb.#{ext}"

  # Inserts the no_match dialyzer attribute to the generated beam to suppress dialyzer warnings
  defp add_dialyze_attribute(beam, compile_flags) do
    case :beam_lib.chunks(to_char_list(beam), [:abstract_code]) do
      {:ok, {_, [abstract_code: :no_abstract_code]}} ->
        # No abstract code, so we can't do anything. Luckily dialyzer can't also run on such beam
        # so we're safe
        :ok
      {:ok, {_, [abstract_code: {_, forms}]}} ->
        new_forms = insert_dialyzer_attr(forms)
        {:ok, _, bytes, _warnings} = :protobuffs_file.compile_forms(new_forms, compile_flags)
        :protobuffs_file.write_file(to_char_list(beam), bytes)
    end
  end

  defp insert_dialyzer_attr([]), do: []
  defp insert_dialyzer_attr([{:attribute, line, :module, _} = module | rest]),
    do: [module, {:attribute, line, :dialyzer, :no_match} | rest]
  defp insert_dialyzer_attr([form | rest]), do: [form | insert_dialyzer_attr(rest)]
end

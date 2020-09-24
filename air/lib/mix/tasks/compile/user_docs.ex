defmodule Mix.Tasks.Compile.UserDocs do
  @shortdoc "Compiles the API documentation."
  @moduledoc "Compiles the API documentation."
  use Mix.Task
  require Logger

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  @impl Mix.Task
  def run(_args) do
    unless System.get_env("INTEGRATION_TESTS") == "true" do
      copy_and_clean_diffix_docs()
    end

    :ok
  end

  # Strips out markdown links of the type [aircloak/aircloak#XXXX](...)
  defp strip_github_links(content),
    do: String.replace(content, ~r/\[aircloak\/aircloak#\d+\]\(.+?\) ?/, "", global: true)

  defp write_processed_contents(content, path) do
    old_content =
      case File.read(path) do
        {:ok, content} -> content
        _ -> nil
      end

    if content != old_content do
      case File.write(path, content) do
        :ok ->
          :ok

        {:error, posix_error} ->
          Mix.raise("Error failed to write #{path}: #{Aircloak.File.humanize_posix_error(posix_error)}")
      end
    end
  end

  defp copy_and_clean_diffix_docs() do
    ~w(diffix.md attacks.md)
    |> Enum.each(fn file_name ->
      with {:ok, contents} <- File.read("../cloak/docs/#{file_name}") do
        contents
        |> strip_github_links()
        |> write_processed_contents("priv/static/docs/#{file_name}")
      else
        {:error, posix_error} ->
          Mix.raise("Error processing cloak doc #{file_name}: #{Aircloak.File.humanize_posix_error(posix_error)}")
      end
    end)
  end
end

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
      update_version_numbers_in_guide()

      if stale?() do
        conditionally_compile_offline_docs()
        cmd!("yarn", ~w(run gitbook build))
        File.mkdir_p!("priv/static")
        File.rm_rf!("priv/static/docs")
        File.cp_r!("docs/_book", "priv/static/docs")
        Mix.Shell.IO.info("Compiled user docs")
      end
    end

    :ok
  end

  defp stale?() do
    source_mtime =
      Path.wildcard("docs/**")
      |> Enum.reject(&(&1 =~ ~r/aircloak-docs/))
      |> Enum.map(&File.stat!(&1).mtime)
      |> Enum.max()

    Path.wildcard("priv/static/docs/**")
    |> Enum.reject(&(&1 =~ ~r/aircloak-docs/))
    |> Enum.map(&File.stat!(&1).mtime)
    |> Enum.sort(&(&1 > &2))
    |> case do
      [] -> true
      [target_mtime | _] -> target_mtime < source_mtime
    end
  catch
    # For some unknown reason, File.stat! fails on a local docker build. Since it is not critical,
    # here we're just suppressing the error and assuming that the target is stale.
    _, _ ->
      true
  end

  defp cmd!(cmd, args) do
    case System.cmd(
           cmd,
           args,
           stderr_to_stdout: true,
           into: IO.stream(:stdio, :line),
           cd: "docs"
         ) do
      {_, 0} ->
        :ok

      {_, _} ->
        Mix.raise("Error building user docs")
    end
  end

  defp conditionally_compile_offline_docs() do
    # We won't build pdfs in static, since this takes quite long, and leads to uncommitted changes. This in turn causes
    # total rebuild of docs on every CI compile, which takes a couple of minutes. The root cause is that pdf/epub/mobi
    # files are committed in the content folder, and accounted for when deciding if there are changes. We need to
    # have a better mechanism here:
    #
    #   - recompiling of pdf/epub/mobi shouldn't lead to uncommitted changes
    #   - these files shouldn't be taken into account when comparing whether something has changed in docs since the
    #     previous build
    #
    # Once we have that in place, we can in fact generate PDFs in CI.
    if has_ebook_convert_installed() and System.get_env("CI") != "true" do
      # Use a readme that doesn't contain links to the offline content
      cmd!("ln", ~w(-sf README-offline.md content/README.md))
      # Ignore offline assets so we don't recursively bundle the offline assets in themselves
      cmd!("ln", ~w(-sf offline-doc-ignores content/.bookignore))

      try do
        cmd!("yarn", ~w(run gitbook pdf ./ content/aircloak-docs.pdf))
        cmd!("yarn", ~w(run gitbook epub ./ content/aircloak-docs.epub))
        cmd!("yarn", ~w(run gitbook mobi ./ content/aircloak-docs.mobi))
      after
        cmd!("ln", ~w(-sf README-online.md content/README.md))
        cmd!("rm", ~w(-f content/.bookignore))
      end
    end
  end

  defp has_ebook_convert_installed() do
    case System.cmd("ebook-convert", ~w(--version)) do
      {_, 0} -> true
      {_, _} -> false
    end
  rescue
    _ -> false
  end

  @book_config_path "docs/book.json"
  @book_summary_path "docs/content/SUMMARY.md"

  defp update_version_numbers_in_guide() do
    current_version = File.read!("../VERSION") |> String.trim()
    update_book_config(current_version)
    update_summary_section(current_version)
  end

  defp update_book_config(current_version) do
    book_config = File.read!(@book_config_path) |> Poison.decode!()

    if String.contains?(book_config["title"], current_version) do
      # All good
    else
      updated_book_config =
        book_config
        |> Map.put("title", "Aircloak User Guide – version #{current_version}")
        |> Map.put(
          "description",
          "This copy of the Aircloak user guide describes the features of, and how to " <>
            "configure and work with Aircloak Insights version #{current_version}."
        )
        |> Poison.encode!(pretty: true)

      File.write(@book_config_path, updated_book_config)
    end
  end

  defp update_summary_section(current_version) do
    original_summary = File.read!(@book_summary_path)

    updated_summary =
      original_summary
      |> String.split("\n")
      |> Enum.map(fn line ->
        if String.contains?(line, "## Aircloak Insights - version") and not String.contains?(line, current_version) do
          "## Aircloak Insights - version #{current_version}"
        else
          line
        end
      end)
      |> Enum.join("\n")

    if original_summary != updated_summary, do: File.write!(@book_summary_path, updated_summary)
  end
end

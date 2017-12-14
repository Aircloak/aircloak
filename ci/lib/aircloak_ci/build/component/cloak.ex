defmodule AircloakCI.Build.Component.Cloak do
  @moduledoc "Implements CI jobs for the Cloak component."

  alias AircloakCI.{Build, Container, LocalProject}
  alias AircloakCI.Build.Job

  @behaviour Job.Compile
  @behaviour Job.StandardTest


  # -------------------------------------------------------------------
  # Job.Compile callbacks
  # -------------------------------------------------------------------

  @impl Job.Compile
  def name(), do: "cloak"

  @impl Job.Compile
  def compile(project, _name, _log_name) do
    with :ok <- build_cloak_image(project), do:
      compile_cloak(LocalProject.ci_version(project), project)
  end


  # -------------------------------------------------------------------
  # Job.StandardTest callbacks
  # -------------------------------------------------------------------

  @impl Job.StandardTest
  def standard_test_job_name(), do: "cloak_test"

  @impl Job.StandardTest
  def standard_test(%{project: project, source: source} = build_state), do:
    Job.maybe_start(
      build_state,
      standard_test_job_name(),
      &start_test(LocalProject.ci_version(project), &1, self(), project, source)
    )

  defp start_test(ci_version, build_state, build_server, project, source) when ci_version >= 3, do:
    Build.Server.start_job(
      build_state,
      standard_test_job_name(),
      fn -> run_standard_test(build_server, project) end,
      report_status: {source.repo, source.sha}
    )
  defp start_test(_ci_version, build_state, _build_server, _project, _source), do:
    build_state


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp build_cloak_image(project) do
    cond do
      LocalProject.ci_version(project) < 4 ->
        build_docker_image(
          project,
          fn ->
            LocalProject.cmd(project, "cloak_docker_build", "ci/scripts/run.sh build_cloak", timeout: :timer.hours(1))
          end
        )

      Container.built?(script(project)) ->
        :ok

      true ->
        build_docker_image(
          project,
          fn -> Container.build(script(project), LocalProject.log_file(project, "cloak_docker_build")) end
        )
    end
  end

  defp build_docker_image(project, fun), do:
    Job.run_queued(:docker_build, project, fun, log_name: "cloak_docker_build")

  defp script(project), do:
    project |> LocalProject.src_folder() |> Path.join("ci/scripts/cloak.sh")

  defp compile_cloak(3, project), do:
    run_in_cloak(project, LocalProject.commands(project, "cloak", :compile))
  defp compile_cloak(ci_version, project) when ci_version >= 4, do:
    Container.with(
      script(project),
      LocalProject.log_file(project, "cloak_compile"),
      &Container.exec(&1, LocalProject.commands(project, "cloak", :compile), timeout: :timer.hours(1))
    )
  defp compile_cloak(_, _project), do: :ok

  defp run_standard_test(build_server, project), do:
    Job.run_queued(:standard_test, project,
      fn ->
        run_test_commands(
          LocalProject.ci_version(project),
          project,
          LocalProject.commands(project, "cloak", :standard_test)
        )
      end,
      job_name: standard_test_job_name(),
      log_name: standard_test_job_name(),
      report_result: build_server,
    )

  defp run_test_commands(3, project, commands), do:
    run_in_cloak(project, commands)
  defp run_test_commands(ci_version, project, commands) when ci_version >= 4 do
    Container.with(
      script(project),
      LocalProject.log_file(project, "cloak_test"),
      fn(cloak) ->
        with :ok <- Container.invoke_script(cloak, "prepare_for_test #{cloak.name}", timeout: :timer.minutes(1)), do:
          Container.exec(cloak, commands, timeout: :timer.minutes(20))
      end
    )
  end

  defp run_in_cloak(project, cmds), do:
    LocalProject.cmd(
        project,
        standard_test_job_name(),
        "ci/scripts/run.sh run_in_cloak_test #{cmds |> Stream.map(&~s("#{&1}")) |> Enum.join(" ")}",
        timeout: :timer.hours(1)
      )
end

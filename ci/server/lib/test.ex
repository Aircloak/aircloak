defmodule Test do
  def run() do
    AircloakCI.Github.repo_data("aircloak", "aircloak") |> IO.inspect
    #
    # AircloakCI.Builder.Server.force_build(AircloakCI.Github.pull_request("aircloak", "aircloak", 2125)) |> IO.inspect
    # AircloakCI.Builder.Server.force_build(AircloakCI.Github.pull_request("aircloak", "aircloak", 2125)) |> IO.inspect
    #AircloakCI.Github.post_comment("sasa1977", "test_repo", 1, "comment1")
  end

  defp pr(), do:
    %{
      approved?: true,
      number: 2117,
      repo: %{owner: "aircloak", name: "aircloak"},
      sha: "19c05351d625b654bad9d6a168ad3e1471c845cf",
      source_branch: "sasa/ci-server",
      status_checks: %{
        "continuous-integration/travis-ci/pr" => :success,
        "continuous-integration/travis-ci/push" => :success
      },
      target_branch: "sasa/extract-compliance-ci",
      title: "foo"
    }
end

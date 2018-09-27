defmodule AircloakCI.ServerTest do
  use AircloakCI.BuildCase, async: true
  alias AircloakCI.Build

  test "job type" do
    assert Build.Server.job_type("cloak_compile") == "compile"
    assert Build.Server.job_type("cloak_test") == "test"
    assert Build.Server.job_type("integration_tests_compile") == "compile"
    assert Build.Server.job_type("integration_tests_test") == "test"
    assert Build.Server.job_type("compliance") == "compliance"
  end
end

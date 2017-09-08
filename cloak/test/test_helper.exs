defmodule TestHelper do
  def excludes(), do:
    Enum.reject([:exclude_in_dev, :compliance], &include?/1)

  defp include?(:exclude_in_dev), do:
    env("TEST_ALL") == "true" || env("TRAVIS") == "true"
  defp include?(:compliance), do:
    env("TEST_ALL") == "true" ||
    env("TRAVIS_EVENT_TYPE") in ["pull_request", "cron"] ||
    env("TRAVIS_BRANCH") == "master" ||
    env("TRAVIS_BRANCH") =~ ~r/^release_.*/

  defp env(name), do:
    System.get_env(name) || ""
end

Cloak.SapHanaHelpers.delete_test_schemas()
Cloak.Test.DB.start_link()
ExUnit.start(exclude: TestHelper.excludes())
ExCheck.start()

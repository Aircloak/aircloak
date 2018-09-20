defmodule DataQuality.Distributions do
  @moduledoc """
  Definitions for beta distributions that should be used for testing the data quality of Aircloak.
  """

  @type distribution :: %{
          min: integer,
          max: integer,
          users: integer,
          entries_per_user: integer,
          alpha: float,
          beta: float
        }

  @distributions [
    %{min: 0, max: 1000, users: 100, entries_per_user: 10, alpha: 2, beta: 2},
    %{min: -1000, max: 1000, users: 1000, entries_per_user: 1, alpha: 0.5, beta: 0.5},
    %{min: -1000, max: 0, users: 1000, entries_per_user: 100, alpha: 1, beta: 3},
    %{min: -10, max: 200, users: 1000, entries_per_user: 1, alpha: 1, beta: 3},
    %{min: 100, max: 800, users: 1000, entries_per_user: 10, alpha: 5, beta: 1},
    %{min: -100, max: 0, users: 10000, entries_per_user: 10, alpha: 2, beta: 5},
    %{min: 100, max: 200, users: 1000, entries_per_user: 1, alpha: 1, beta: 1},
    %{min: 0, max: 200, users: 100, entries_per_user: 100, alpha: 2, beta: 3}
  ]

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @spec list() :: [distribution]
  @doc "Returns a list of distribution parameters"
  def list(), do: @distributions

  @spec distribution_name(distribution) :: String.t()
  @doc "Returns a distribution unique name for a distribution"
  def distribution_name(distribution),
    do:
      "beta[a=#{distribution[:alpha]} b=#{distribution[:beta]}] from " <>
        "#{distribution[:min]} to #{distribution[:max]} " <>
        "(#{distribution[:users]} users with #{distribution[:entries_per_user]})"
end

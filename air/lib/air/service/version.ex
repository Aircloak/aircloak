defmodule Air.Service.Version do
  @moduledoc "Services for verifying whether or not the current Aircloak version is up to date."

  #-----------------------------------------------------------------------------------------------------------
  # API functions
  #-----------------------------------------------------------------------------------------------------------

  @doc "Returns true if the version has expired"
  @spec expired?() :: boolean
  def expired?(), do: days_until_expiry() <= 0

  @doc "Returns true if the software will expire in less than a month"
  @spec expiry_eventually?() :: boolean
  def expiry_eventually?(), do: days_until_expiry() < 30

  @doc "Returns true if the software will expire in less than two weeks"
  @spec expiry_imminent?() :: boolean
  def expiry_imminent?(), do: days_until_expiry() < 14

  @doc "Returns true if the software will expire in less than a week"
  @spec expiry_critical?() :: boolean
  def expiry_critical?(), do: days_until_expiry() < 7

  @doc "The number of days until the version expires"
  @spec days_until_expiry() :: integer
  def days_until_expiry() do
    {days, _time} = :calendar.time_difference(
      date_to_erlang(Date.utc_today()),
      date_to_erlang(expiry_date())
    )
    days
  end

  @doc "Returns the date at which the system will expire."
  @spec expiry_date() :: Date.t
  def expiry_date(), do:
    Application.fetch_env!(:air, Air.Service.Version)
    |> Keyword.get(:version_expiry)

  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp date_to_erlang(date) do
    erlang_date = Date.to_erl(date)
    {erlang_date, {0,0,0}}
  end
end

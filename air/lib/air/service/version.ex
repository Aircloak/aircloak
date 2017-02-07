defmodule Air.Service.Version do
  @moduledoc "Services for verifying whether or not the current Aircloak version is up to date."

  @type expiry_status :: :valid | :expired | :will_expire | :expires_shortly | :imminent


  #-----------------------------------------------------------------------------------------------------------
  # API functions
  #-----------------------------------------------------------------------------------------------------------

  @doc "Returns true if the version has expired"
  @spec expired?() :: boolean
  def expired?(), do: expiry_status() == :expired

  @doc "Returns information about the state of the current version."
  @spec expiry_status() :: expiry_status
  def expiry_status() do
    days_until_expiry = days_until_expiry()
    cond do
      days_until_expiry < 0 -> :expired
      days_until_expiry < 7 -> :imminent
      days_until_expiry < 14 -> :expires_shortly
      days_until_expiry < 30 -> :will_expire
      true -> :valid
    end
  end

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

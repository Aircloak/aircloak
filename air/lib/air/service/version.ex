defmodule Air.Service.Version do
  @moduledoc "Services for verifying whether or not the current Aircloak version is up to date."

  @type expiry_status :: :valid | :expired | :will_expire | :expires_shortly | :imminent


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

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
      # Credo wrongly reports that we're missing spaces around operators in this function.
      # credo:disable-for-next-line Credo.Check.Consistency.SpaceAroundOperators
      days_until_expiry < 30 -> :will_expire
      true -> :valid
    end
  end

  @doc "The number of days until the version expires"
  @spec days_until_expiry() :: integer
  def days_until_expiry(), do: Timex.diff(expiry_date(), Date.utc_today, :days)

  @doc "Returns the date at which the system will expire."
  @spec expiry_date() :: Date.t
  def expiry_date(), do:
    Application.fetch_env!(:air, Air.Service.Version)
    |> Keyword.get(:version_expiry)
end

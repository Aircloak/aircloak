Cloak.SapHanaHelpers.delete_test_schemas()
Cloak.Test.DB.start_link()
ExUnit.start(exclude: [:exclude_in_dev, :compliance, :pending, :fuzz],
  case_load_timeout: :timer.minutes(5), timeout: :timer.minutes(5))
ExCheck.start()

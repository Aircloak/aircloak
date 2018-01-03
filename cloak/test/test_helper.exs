Compliance.Runtime.start_link()
Cloak.SapHanaHelpers.delete_test_schemas()
Cloak.Test.DB.start_link()
ExUnit.start(exclude: [:exclude_in_dev, :compliance, :pending],
  case_load_timeout: :timer.minutes(5), timeout: :timer.minutes(5))
ExCheck.start()
System.at_exit(fn(_) -> Compliance.Runtime.finalize() end)

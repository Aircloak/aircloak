File.rm(Cloak.DataSource.Isolators.PersistentKeyValue.cache_file())
File.rm(Cloak.DataSource.Shadows.PersistentKeyValue.cache_file())
File.rm(Cloak.DataSource.Bounds.PersistentKeyValue.cache_file())

Compliance.Runtime.start_link()
Cloak.SapHanaHelpers.delete_test_schemas()
Cloak.Test.DB.start_link()

ExUnit.start(
  exclude: [:exclude_in_dev, :compliance, :pending],
  case_load_timeout: :timer.minutes(5),
  timeout: :timer.minutes(5)
)

System.at_exit(fn _ -> Compliance.Runtime.finalize() end)

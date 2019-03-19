File.rm_rf("browser_test/screenshots")
ExUnit.start(exclude: [:acceptance])

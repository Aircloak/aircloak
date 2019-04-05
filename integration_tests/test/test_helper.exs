File.rm_rf("browser_test/screenshots")
File.mkdir_p("browser_test/screenshots")
ExUnit.start(exclude: [:acceptance])

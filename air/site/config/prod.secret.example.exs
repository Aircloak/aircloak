use Mix.Config

# You can use this file as the template for your local `prod.secret.exs`.
# Do not use these values in production. Instead, you should generate some
# other values, and keep them out of the source control.

config :air, Air.Endpoint,
  secret_key_base: "7nOn9WDwUZLjXnQah8+ocSF0r/YhYpDp0hSw4FE6Zq+Ic2TVYcDCQe+2MT8wqD/R"

config :guardian, Guardian,
  secret_key: "6MvtANFkxCr3VaDY/C8oCooF6Pg1uqFzOWNYVMry/V5acmSPuQydPeU5X5Jh"

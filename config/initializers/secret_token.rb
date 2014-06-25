# Be sure to restart your server when you modify this file.

# Your secret key for verifying the integrity of signed cookies.
# If you change this key, all old signed cookies will become invalid!

# Make sure the secret is at least 30 characters and all random,
# no regular words or you'll be exposed to dictionary attacks.
# You can use `rake secret` to generate a secure secret key.

raise "secret_key_base is missing from settings.local.yml" unless Rails.configuration.secret_key_base
Web::Application.config.secret_key_base = Rails.configuration.secret_key_base

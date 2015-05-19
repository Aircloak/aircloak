require './lib/aircloak_config'

# Be sure to restart your server when you modify this file.

# Your secret key for verifying the integrity of signed cookies.
# If you change this key, all old signed cookies will become invalid!

# Make sure the secret is at least 30 characters and all random,
# no regular words or you'll be exposed to dictionary attacks.
# You can use `rake secret` to generate a secure secret key.

secret_key_base = nil
begin
  secret_key_base = Conf.get("/settings/rails/secrets/secret_key_base")
rescue Exception => e
  puts "Could not load secret_key_base from ETCD. OK if during asset precompile"
end

Web::Application.config.secret_key_base = secret_key_base

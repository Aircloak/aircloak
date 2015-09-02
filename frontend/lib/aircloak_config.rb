require 'etcd'
require 'yaml'

class Conf
  @@setup = false
  @@client = nil

  def self.get path
    # In precompile env we just compile assets, so we don't want to connect to etcd (it's possible that it's
    # not running)
    return "" if Rails.env == "precompile"

    setup
    @@client.get(path).value
  end

  # Utility function to avoid false negatives/positives around the
  # /settings/rails/global key
  def self.production_mode?
    get("/settings/rails/global") == "true"
  end

private
  def self.setup
    return if @@setup

    @@client = Etcd.client(host: "127.0.0.1", port: port)
    @@setup = true
  end

  def self.port
    case Rails.env
      # In dev/test, we get the port via bash script. This is a convenience
      # hack which allows us to normally use `rake`, `rails`, and friends, without
      # needing to set env variable ourselves.
      when "development" then `./etcd_client_port.sh dev`
      when "test" then `./etcd_client_port.sh test`

      # In production the port is always provided via OS env. This is an
      # implementation detail that allows us to reuse the code from
      # `config/config.sh` without needing to run bash script from Rails.
      else ENV['ETCD_CLIENT_PORT']
    end
  end
end

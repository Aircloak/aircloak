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
      when "development" then `./etcd_client_port.sh dev`
      when "test" then `./etcd_client_port.sh test`
      else ENV['ETCD_CLIENT_PORT']
    end
  end
end

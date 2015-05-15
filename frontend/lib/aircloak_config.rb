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

private
  def self.setup
    return if @@setup

    unless Rails.env == "test"
      host = ENV['ETCD_HOST']
      port = ENV['ETCD_PORT']

      # In dev, we don't require env vars. If they're not present, we just use local ETCD
      if Rails.env == "development"
        host ||= "127.0.0.1"
        port ||= "4001"
      end
    else
      # Hardcoded test ETCD endpoint
      host = "127.0.0.1"
      port = "4002"
    end

    @@client = Etcd.client(host: host, port: port)
    @@setup = true
  end
end

require 'etcd'
require 'yaml'

class Conf
  @@setup = false
  @@client = nil

  def self.get path
    setup
    @@client.get(path).value
  end

private
  def self.setup
    return if @@setup
    unless Rails.env == "test"
      host = ENV['ETCD_HOST'] || "127.0.0.1"
      port = ENV['ETCD_PORT'] || 4001
    else
      host = "127.0.0.1"
      port = "4002"
    end
    @@client = Etcd.client(host: host, port: port)
    @@setup = true
  end
end

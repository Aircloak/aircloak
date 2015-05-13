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
      config = YAML.load_file('config/etcd_config.yml')
      host = config["connection"]["host"]
      port = config["connection"]["port"]
    else
      host = "127.0.0.1"
      port = "4002"
    end
    @@client = Etcd.client(host: host, port: port)
    @@setup = true
  end
end

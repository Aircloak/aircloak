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
    config = YAML.load_file('config/etcd_config.yml')
    host = config["connection"]["host"]
    port = config["connection"]["port"]
    @@client = Etcd.client(host: host, port: port)
    @@setup = true
  end
end

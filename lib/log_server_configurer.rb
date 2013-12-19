require './lib/proto/air/log_config_messages.pb'
require './lib/protobuf_sender'

class LogServerConfigurer
  def self.proto_for_cluster cluster
    LogConfigPB::ClusterPB.new(
      cluster_name: cluster.log_name,
      cloak_names: cluster.cloaks.map(&:name)
    )
  end

  def self.proto
    LogConfigPB.new(
      clusters: Cluster.all.each.map {|cluster| proto_for_cluster cluster},
      unassigned_cloaks: Cluster.available_cloaks.map(&:name)
    )
  end
  
  def self.server_url
    "http://#{Rails.configuration.log_server.host}/config"
  end

  def self.update_config
    ProtobufSender.post_to_url server_url, proto
  end
end

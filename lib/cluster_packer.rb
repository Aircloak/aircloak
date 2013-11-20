require './lib/proto/air/management_messages.pb'

class ClusterPacker
  def self.package_cluster cluster
    ClusterProto.new(
      timestamp: cluster.timestamp,
      cluster_id: cluster.id,
      machines: (package_machines cluster)
    )
  end

private
  def self.package_machines cluster
    cluster.cluster_cloaks.map do |cluster_cloak|
      ClusterProto::MachineProto.new(
        machine_id: cluster_cloak.cloak.id,
        state: cluster_cloak.proto_state
      )
    end
  end
end

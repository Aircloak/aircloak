require './lib/proto/air/management_messages.pb'

class ClusterPacker
  def self.package_cluster cluster
    crts, crls = cluster.get_client_credentials
    ClusterPB.new(
      timestamp: cluster.timestamp,
      id: cluster.id,
      members: (package_members cluster),
      name: cluster.name,
      client_crts: crts,
      client_crls: crls
    )
  end

  def self.package_clusters clusters
    ClustersPB.new(clusters: clusters.map {|cluster| package_cluster cluster })
  end

private
  def self.package_members cluster
    cluster.cluster_cloaks.map do |cluster_cloak|
      ClusterPB::MemberPB.new(
        machine_id: cluster_cloak.cloak.id,
        state: cluster_cloak.proto_state
      )
    end
  end
end

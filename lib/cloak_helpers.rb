class CloakHelpers

  def self.cloak_url task, path
    some_cloak = some_cloak task
    raise "No cloak in cluster" unless some_cloak
    prot = Rails.configuration.cloak.protocol
    port = Rails.configuration.cloak.port
    return "#{prot}://#{some_cloak.ip}:#{port}/#{path}"
  end

  def self.some_cloak task
    cluster_cloak = ClusterCloak.where(cluster_id: task.cluster_id, raw_state: ClusterCloak.state_to_raw_state(:belongs_to)).limit(1).first
    cluster_cloak.cloak if cluster_cloak
  end

end

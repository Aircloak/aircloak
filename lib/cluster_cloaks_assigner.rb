class ClusterCloaksAssigner
  def self.get_cloaks params
    return [] unless params["cloak_selections"]
    cloak_ids = params["cloak_selections"].map(&:to_i)
    cloak_ids.map {|id| Cloak.find(id)}
  end

  def self.assign_from_list cluster, new_cloaks
    old_cloaks = cluster.cloaks.to_a
    (new_cloaks - old_cloaks).each {|cloak| cluster.cloaks << cloak }
    (old_cloaks - new_cloaks).each {|cloak| cloak.cluster_cloak.destroy}
  end
end

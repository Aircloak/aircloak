class ClusterCloak < ActiveRecord::Base
  belongs_to :cluster
  belongs_to :cloak

  validate :matching_tpm_for_cloak_and_cluster

  def matching_tpm_for_cloak_and_cluster
    self.errors.add :cloak, "must match tpm configuration" unless self.cloak.tpm == self.cluster.tpm
  end
end

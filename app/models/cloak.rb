class Cloak < ActiveRecord::Base
  has_one :cluster_cloak
  has_one :cluster, through: :cluster_cloak
  validates :ip, format: { with: /\A(((25[0-5])|(2[0-4][0-9])|([01]?[0-9][0-9]?))\.){3}((25[0-5])|(2[0-4][0-9])|([01]?[0-9][0-9]?))/}
  validates_presence_of :name
  validates_uniqueness_of :name, :ip

  def self.health_types
    health_mappings.values
  end

  def health
    Cloak.health_mappings[raw_health]
  end

  def set_health health
    self.raw_health = Cloak.health_mappings.invert[health]
  end

  def display_name
    name + (tpm ? "" : " (no tpm)")
  end

  def tpm_display
    tpm ? "TPM" : "no TPM"
  end

  def self.all_unassigned
    Cloak.includes(:cluster_cloak).where(cluster_cloaks: { id: nil })
  end

  def self.all_assigned_sorted
    Cloak.includes(:cluster_cloak).where.not(cluster_cloaks: { id: nil }).sort do |a, b|
      case a.cluster.name <=> b.cluster.name
      when -1 then -1
      when 0 then a.name <=> b.name
      when 1 then 1
      end
    end
  end

private
  def self.health_mappings
    {
      nil => :unknown, 
      0 => :good, 
      1 => :changing, 
      2 => :sw_failing,
      3 => :hw_failing
    }
  end
end

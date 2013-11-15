class Cloak < ActiveRecord::Base
  has_one :cluster_cloak
  has_one :cluster, through: :cluster_cloak
  validates :ip, format: { with: /\A(((25[0-5])|(2[0-4][0-9])|([01]?[0-9][0-9]?))\.){3}((25[0-5])|(2[0-4][0-9])|([01]?[0-9][0-9]?))/}
  validates_presence_of :name
  validates_uniqueness_of :name, :ip
  validates_inclusion_of :raw_health, :in => 0..3

  def health
    case raw_health
    when 0 then :good
    when 1 then :changing
    when 2 then :sw_failing
    when 3 then :hw_failing
    end
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
end

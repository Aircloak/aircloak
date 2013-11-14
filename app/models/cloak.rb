class Cloak < ActiveRecord::Base
  has_one :cluster_cloak
  has_one :cluster, through: :cluster_cloak
  validates :ip, format: { with: /\A(((25[0-5])|(2[0-4][0-9])|([01]?[0-9][0-9]?))\.){3}((25[0-5])|(2[0-4][0-9])|([01]?[0-9][0-9]?))/}
  validates_presence_of :name
  validates_uniqueness_of :name, :ip

  def health
    case raw_health
    when 0 then :ok
    when 1 then :changing
    when 2 then :down
    when 3 then :unavailable
    else :unknown
    end
  end

  def display_name
    name + (tpm ? "" : " (no tpm)")
  end

  def tpm_display
    if tpm
      "TPM"
    else
      "no TPM"
    end
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

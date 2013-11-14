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
    else :unknown
    end
  end

  def display_name
    name + (tpm ? "" : " (no tpm)")
  end

  def self.all_unassigned
    Cloak.includes(:cluster_cloak).where(cluster_cloaks: { id: nil })
  end
end

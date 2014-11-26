require './lib/proto/air/management_messages.pb'
require './lib/protobuf_sender'
require './lib/machine_packer'

class NotEnoughCloaks < Exception; end

class Cloak < ActiveRecord::Base
  has_one :cluster_cloak
  has_one :cluster, through: :cluster_cloak
  validates :ip, format: { with: /\A(((25[0-5])|(2[0-4][0-9])|([01]?[0-9][0-9]?))\.){3}((25[0-5])|(2[0-4][0-9])|([01]?[0-9][0-9]?))/}
  validates_presence_of :name
  validates_uniqueness_of :name, :ip
  before_destroy :validate_destroyability

  def display_name
    name + (tpm ? "" : " (no tpm)")
  end

  def tpm_display
    tpm ? "TPM" : "no TPM"
  end

  def set_broken
    self.good = false
    self.save
  end

  def can_destroy?
    !cluster_cloak
  end

  def self.all_unassigned
    Cloak.includes(:cluster_cloak).where(cluster_cloaks: { id: nil })
  end

  def self.all_available
    Cloak.includes(:cluster_cloak).where(cluster_cloaks: { id: nil }, good: true)
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

  def health
    good ? "good" : "poor"
  end

  def internal_domain
    "#{name}.mpi-sws.org"
  end

  def aircloak_domain
    "#{name}.cloak.aircloak.net"
  end

private
  def validate_destroyability
    if not can_destroy?
      self.errors.add(:cluster_cloak, "cannot destroy a cloak assigned to a cluster")
      return false
    end
  end
end

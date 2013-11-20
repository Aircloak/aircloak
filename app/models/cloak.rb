require './lib/proto/air/management_messages.pb'
require './lib/protobuf_sender'
require './lib/machine_packer'

class Cloak < ActiveRecord::Base
  has_one :cluster_cloak
  has_one :cluster, through: :cluster_cloak
  validates :ip, format: { with: /\A(((25[0-5])|(2[0-4][0-9])|([01]?[0-9][0-9]?))\.){3}((25[0-5])|(2[0-4][0-9])|([01]?[0-9][0-9]?))/}
  validates_presence_of :name
  validates_uniqueness_of :name, :ip
  after_create :create_inform_mannyair
  before_destroy :validate_destroyability
  after_destroy :destroy_inform_mannyair

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

private
  def validate_destroyability
    if not can_destroy?
      self.errors.add(:cluster_cloak, "cannot destroy a cloak assigned to a cluster")
      return false
    end
  end

  def mannyair_post_url
    "http://#{ProtobufSender.mannyair_host}/machines"
  end

  def mannyair_delete_url id
    "http://#{ProtobufSender.mannyair_host}/machines/#{id}"
  end

  def create_inform_mannyair
    mp = MachinePacker.package_cloak self
    ProtobufSender.post_to_url mannyair_post_url, mp
  end

  def destroy_inform_mannyair
    Net::HTTP.delete(mannyair_delete_url id)
  end
end

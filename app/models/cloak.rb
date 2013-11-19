require './lib/proto/air/management_messages.pb'

class Cloak < ActiveRecord::Base
  has_one :cluster_cloak
  has_one :cluster, through: :cluster_cloak
  validates :ip, format: { with: /\A(((25[0-5])|(2[0-4][0-9])|([01]?[0-9][0-9]?))\.){3}((25[0-5])|(2[0-4][0-9])|([01]?[0-9][0-9]?))/}
  validates_presence_of :name
  validates_uniqueness_of :name, :ip
  after_create :create_inform_mannyair
  after_destroy :destroy_inform_mannyair

  def display_name
    name + (tpm ? "" : " (no tpm)")
  end

  def tpm_display
    tpm ? "TPM" : "no TPM"
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
  def mannyair_machine
    "manny-air.aircloak.com"
  end

  def mannyair_post_url
    "http://#{mannyair_machine}/machines"
  end

  def mannyair_delete_url id
    "http://#{mannyair_machine}/machines/#{id}"
  end

  def create_inform_mannyair
    mp = MachineProto.new(
      machine_id: id,
      name: name,
      type: tpm ? MachineProto::MachineType::PHYSICAL : MachineProto::MachineType::VM,
      good: good
    )
    ProtobufSender.post_to_url mannyair_post_url, mp
  end

  def destroy_inform_mannyair
    Net::HTTP.delete(mannyair_delete_url id)
  end
end

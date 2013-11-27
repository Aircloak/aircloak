require './lib/proto/air/management_messages.pb'

class ClusterCloak < ActiveRecord::Base
  belongs_to :cluster
  belongs_to :cloak

  validates_presence_of :raw_state
  validates :raw_state, :inclusion => 1..3

  def state
    ClusterCloak.state_map[raw_state]
  end

  def set_state new_state
    self.update(raw_state: ClusterCloak.state_map.invert[new_state])
    cluster.cloak_ready if new_state == :belongs_to
  end

  def proto_state
    ClusterCloak.proto_state_map[raw_state]
  end

  def synchronize
    case state
    when :to_be_added then
      set_state :belongs_to
    when :to_be_removed then
      temp_cluster = cluster
      destroy
      temp_cluster.destroy if temp_cluster.cloaks.count == 0
    end
    save
  end

private
  def self.state_map
    {
      1 => :to_be_added,
      2 => :belongs_to,
      3 => :to_be_removed
    }
  end

  def self.proto_state_map
    {
      1 => ClusterProto::MachineState::TO_BE_ADDED,
      2 => ClusterProto::MachineState::BELONGS_TO,
      3 => ClusterProto::MachineState::TO_BE_REMOVED
    }
  end
end

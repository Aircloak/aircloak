require './lib/proto/air/management_messages.pb'

class ClusterCloak < ActiveRecord::Base
  belongs_to :cluster
  belongs_to :cloak

  validates_presence_of :raw_state
  validates :raw_state, :inclusion => 1..4

  def state
    ClusterCloak.state_map[raw_state]
  end

  def set_state new_state
    self.update(raw_state: ClusterCloak.state_map.invert[new_state])
  end

  def proto_state
    ClusterCloak.proto_state_map[raw_state]
  end

  def synchronize
    case state
    when :to_be_added, :to_be_upgraded then
      set_state :belongs_to
    when :to_be_removed then
      temp_cluster = cluster
      cloak.audit_logs.destroy_all
      destroy
      if temp_cluster.cloaks.count == 0
        # This is used by the integration tests to know when to move on
        connection.execute "NOTIFY cluster_destroyed, '#{temp_cluster.id}'"
        temp_cluster.destroy
      end
    end
    save
  end

  def self.state_to_raw_state state
    ClusterCloak.state_map.invert[state]
  end

private
  def self.state_map
    {
      1 => :to_be_added,
      2 => :belongs_to,
      3 => :to_be_removed,
      4 => :to_be_upgraded
    }
  end

  def self.proto_state_map
    {
      1 => ClusterPB::MachineState::TO_BE_ADDED,
      2 => ClusterPB::MachineState::BELONGS_TO,
      3 => ClusterPB::MachineState::TO_BE_REMOVED,
      4 => ClusterPB::MachineState::TO_BE_UPGRADED
    }
  end
end

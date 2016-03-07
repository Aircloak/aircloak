class CapabilityCluster < ActiveRecord::Base
  belongs_to :cluster
  belongs_to :capability
end

class ClusterCloak < ActiveRecord::Base
  belongs_to :cluster
  belongs_to :cloak
end

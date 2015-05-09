class RaTaskCodeCluster < ActiveRecord::Base
  belongs_to :ra_task_code
  belongs_to :cluster
end

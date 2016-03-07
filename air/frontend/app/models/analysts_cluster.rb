class AnalystsCluster < ActiveRecord::Base
  belongs_to :cluster
  belongs_to :analyst
end

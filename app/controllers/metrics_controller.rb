class MetricsController < ApplicationController
  filter_resource_access
  protect_from_forgery :except => :event

  def index
    @clusters = Cluster.all(:include => :cloaks)
    @cluster_cloaks =
      @clusters.inject({}) do |memo, cluster|
        memo.merge(cluster.name => cluster.cloak_names)
      end.
      to_json
  end
end
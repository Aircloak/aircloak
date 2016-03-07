class MetricsController < ApplicationController
  filter_resource_access
  protect_from_forgery :except => :event

  def index
    @clusters = Cluster.all.includes(:cloaks) # eager loading to minimize number of DB queries

    # Creates a cluster -> cloaks mapping, that will ultimately be used on the client side
    @cluster_cloaks =
      @clusters.inject({}) do |memo, cluster|
        memo.merge(cluster.name => cluster.cloak_internal_domains)
      end.
      to_json
  end
end

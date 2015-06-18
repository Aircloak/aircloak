class Api::ClustersController < ApplicationController
  filter_access_to [:index], require: :anon_read
  before_action :authenticate_api_user
  respond_to :json

  def set_layout
    self.class.layout false
  end

  # GET /api/clusters
  def index
    clusters = @current_user.analyst.clusters.map do |cluster|
      {
        id: cluster.id,
        name: cluster.name
      }
    end
    respond_with({success: true, clusters: clusters}, status: :ok)
  end
end

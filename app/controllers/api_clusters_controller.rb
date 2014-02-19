require './lib/proto/air/management_messages.pb'

class ApiClustersController < ApplicationController
  filter_access_to :status, require: :anon_write
  protect_from_forgery :except => :status

  def status
    cs = ClusterStatusPB.decode(request.body.read)
    cluster = Cluster.find(params[:id])
    cluster.status_description = cs.description
    cluster.status = case cs.status
      when ClusterStatusPB::Status::ACTIVE
        :active
      when ClusterStatusPB::Status::IN_SERVICE
        :in_service
      when ClusterStatusPB::Status::INACTIVE
        :inactive
    end
    cluster.save
    render :text => "thanks", layout: false
  end
end

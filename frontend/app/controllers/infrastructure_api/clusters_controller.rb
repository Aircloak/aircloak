require './lib/proto/air/management_messages.pb'

class InfrastructureApi::ClustersController < ApplicationController
  filter_access_to :status, require: :anon_write
  protect_from_forgery :except => :status

  def status
    cs = ClusterStatusPB.decode(request.raw_post)
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
    ClusterMailer.status_mail(cluster, cs).deliver unless cluster.name =~ /^test-/
    if cs.status == ClusterStatusPB::Status::ACTIVE
      cluster.check_capabilities 
      cluster.last_active = Time.now
    end
    cluster.save
    render :text => "thanks", layout: false
  end
end

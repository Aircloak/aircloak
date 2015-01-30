class InfrastructureApi::BuildProgressController < ApplicationController
  filter_access_to [:version_progress, :build_progress], require: :anon_write
  protect_from_forgery :except => [:version_progress, :build_progress]

  def version_progress
    r = VersionBuildResponseProto.decode(request.raw_post)
    version = DeployableEntityVersion.find_by_commit_id(r.commit_id)
    if version then
      version.build_log = r.log_output
      version.build_completed = true
      unless version.build_success == false
        version.build_success = r.status == VersionBuildResponseProto::Status::OK
      end
      version.save
    end
    render text: "Version on!", layout: false
  end

  def build_progress
    r = BuildResponseProto.decode(request.raw_post)
    build = Build.find(r.build_id)
    success = case r.status
      when BuildResponseProto::Status::OK then true
      when BuildResponseProto::Status::ERROR then false
    end
    build.mark_complete success: success
  rescue ActiveRecord::RecordNotFound
  ensure
    render text: "Fab! Thanks!", layout: false
  end
end

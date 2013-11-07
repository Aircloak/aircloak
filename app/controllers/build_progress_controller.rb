class BuildProgressController < ApplicationController
  filter_access_to [:version_progress, :build_progress], require: :anon_write
  protect_from_forgery :except => [:version_progress, :build_progress]

  def version_progress
    r = VersionBuildResponseProto.decode(request.body.read)
    version = DeployableEntityVersion.find_by_commit_id(r.commit_id)
    if version then
      if version.deployable_entity.tpm_env == r.environment then
        version.build_log_tpm = r.log_output
      end
      if version.deployable_entity.no_tpm_env == r.environment then
        version.build_log_no_tpm = r.log_output
      end
      # This is somewhat of a special case.
      # If a deployable entity version compilation
      # fails before the build server got started on 
      # the per environment specializations,
      # it does not yet know about which environments
      # exist, and therefore cannot report back per
      # environment, but instead reports a shared log
      # feedback entry.
      if r.environment == "shared" then
        version.build_log_tpm = r.log_output
        version.build_log_no_tpm = r.log_output
      end
      version.build_completed = true
      unless version.build_success == false
        version.build_success = r.status == VersionBuildResponseProto::Status::OK
      end
      version.save
    end
    render text: "Version on!", layout: false
  end

  def build_progress
    r = BuildResponseProto.decode(request.body.read)
    build = Build.find(r.build_id)
    build.build_completed = true
    if r.status == BuildResponseProto::Status::OK then
      build.build_success = true
    end
    if r.status == BuildResponseProto::Status::ERROR then
      build.build_success = false
    end
    build.save
  rescue ActiveRecord::RecordNotFound
  ensure
    render text: "Fab! Thanks!", layout: false
  end
end

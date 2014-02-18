require './lib/build_manager'

class BuildVersionsAssigner
  def self.assign_versions build, versions
    versions.each do |version|
      dev = DeployableEntityVersion.find(version)
      build.deployable_entity_versions << dev
    end
  end

  def self.assign_from_params build, params
    if params["from_develop"] == "true" then
      build.deployable_entity_versions << BuildManager.find_right_versions(DeployableEntity.all)
    else
      return unless params["build_versions"]
      version_ids = params["build_versions"].map(&:to_i)
      assign_versions build, version_ids
    end
  end
end

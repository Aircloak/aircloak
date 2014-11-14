require './lib/build_manager'

class BuildVersionsAssigner
  def self.assign_from_params build, params
    if params["from_develop"] == "true" then
      build.deployable_entity_versions << BuildManager.find_right_versions(DeployableEntity.all)
    else
      return unless params["branch_selections"]
      # The branch_selections param contains an array of objects
      # where each object contains:
      # - id: id of rails deployable entity
      # - repo: repo name, just for good measure, and easier debugging
      # - sha: commit of the head of the particular branch
      repo_commits = JSON.parse params["branch_selections"]
      repo_commits.each do |info|
        de = DeployableEntity.find info["id"]
        de.add_commit info["sha"]
        build.deployable_entity_versions << DeployableEntityVersion.find_by_commit_id(info["sha"])
      end
    end
  end
end

class RegisterVersionController < ApplicationController
  filter_access_to :create, require: :anon_write
  protect_from_forgery :except => :create 

  def create
    payload = JSON.parse(params["payload"])
    repo = payload["repository"]["name"]
    commit_id = payload["commit"]

    deployable_entity = DeployableEntity.find_by_repo(repo)
    deployable_entity.add_commit commit_id if deployable_entity

    render text: "Awesome, thanks!", layout: false
  end
end

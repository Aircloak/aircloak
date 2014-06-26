require 'digest/sha2'

class RegisterVersionController < ApplicationController
  filter_access_to :create, require: :anon_write
  protect_from_forgery :except => :create

  def create
    payload = JSON.parse(params["payload"])
    repo = payload["repository"]["name"]
    commit_id = payload["commit"]

    digest = Digest::SHA2.new.update("aircloak/#{repo}#{Rails.configuration.travis_token}")

    if digest.to_s == request.headers["Authorization"]
      deployable_entity = DeployableEntity.find_by_repo(repo)
      deployable_entity.add_commit commit_id if deployable_entity
      render text: "Awesome, thanks!", layout: false
    else
      render text: "Unauthorized request. Invalid Travis authorization header!", layout: false, status: 401
    end
  end
end

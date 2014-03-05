class VersionTestsController < ApplicationController
  filter_access_to :update, require: :anon_write
  filter_access_to :create_local, require: :manage
  protect_from_forgery :except => :update 
  def index
    @version_tests = VersionTest.all.order(created_at: :asc)
  end

  def show
    @version_test = VersionTest.find(params[:id])
  end

  def destroy
    VersionTest.find(params[:id]).destroy
    redirect_to version_tests_path
  end

  def update
    test = VersionTest.find(params[:id])
    test_result = TestResponsePB.decode(request.raw_post)
    test.process_result test_result
    render text: "Finally!", layout: false
  rescue ActiveRecord::RecordNotFound
    render text: "Don't know this test...", status: 404, layout: false
  end

  def create_local
    # clean the database and create the cloak, deployable entity, and deployable entity version
    Query.destroy_all
    ClusterCloak.delete_all
    Cloak.delete_all
    Cluster.delete_all
    VersionTest.delete_all
    OsTag.delete_all
    Build.delete_all
    DeployableEntityVersion.delete_all
    DeployableEntity.delete_all
    c = Cloak.create!(name: "localhost", ip: "127.0.0.1", tpm: false)
    ot = OsTag.create!(name: "ostag", description: "ostag")
    de = DeployableEntity.create!(repo: "repo", tpm_env: "tpm_env", no_tpm_env: "no_tpm_env")
    de.add_commit "deadbeef"
    redirect_to version_tests_path
  end
end

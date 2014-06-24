require './lib/build_manager.rb'
require './lib/gh.rb'

describe BuildManager do
  before(:each) do
    begin DeployableEntity
    rescue
      class DeployableEntity; end
    end
    begin DeployableEntityVersion
    rescue
      class DeployableEntityVersion; end
    end
    begin Build
    rescue
      class Build; end
    end
  end

  it "should create a build request from a build" do
    entity1 = double(:entity1, repo: "erlattest")
    entity2 = double(:entity2, repo: "cloak-core")
    versions = [
      double(:version1, commit_id: "commit1", deployable_entity: entity1),
      double(:version2, commit_id: "commit2", deployable_entity: entity2)
    ]
    build = double(
      :build, 
      id: 1, 
      name: "test-build", 
      tpm: true,
      deployable_entity_versions: versions
    )
    req = BuildManager.create_build_request build
    req.build_name.should eq build.name
    req.build_id.should eq build.id
    req.versions.size.should eq versions.size
    versions.each_index do |i|
      req.versions[i].commit_id.should eq versions[i].commit_id
      req.versions[i].repo.should eq versions[i].deployable_entity.repo
    end
  end

  it "should be able to list all entities excluding one" do
    this_one = double(id: 1)
    the_other = double(id: 2)
    DeployableEntity.should_receive(:where).with("id != (?)", 1).and_return([the_other])
    BuildManager.all_entities_except(this_one).should eq [the_other]
  end

  it "should find and ensure all versions of entities required exist" do
    version = double
    DeployableEntityVersion.stub(find_by_commit_id: version)
    entity = double(add_commit: true, repo: "erlattest")
    sha = "some-sha"
    Gh.stub(:latest_commit_on_branch_for_repo).and_return(sha)
    BuildManager.find_right_versions([entity]).should eq([version])
  end

  it "should create a build consisting of the required entity versions" do
    entity = double(add_commit: true, repo: "erlattest", id: 1)
    version = double(deployable_entity: entity, message: "Message")
    DeployableEntity.should_receive(:where).with("id != (?)", 1).and_return([])
    build = double
    Build.should_receive(:new).with(tpm: false, name: "testbuild [erlattest] - Message", deployable_entity_versions: [version]).and_return build
    BuildManager.test_build_for_version(version).should eq build
  end
end

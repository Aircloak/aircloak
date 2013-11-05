require './lib/build_manager.rb'

describe BuildManager do
  it "should create a build request from a build" do
    entity1 = double(:entity1, repo: "erlattest", tpm_env: "tpm", no_tpm_env: "no-tpm")
    entity2 = double(:entity2, repo: "cloak-core", tpm_env: "standard", no_tpm_env: "standard")
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
      req.versions[i].environment.should eq versions[i].deployable_entity.tpm_env
      req.versions[i].commit_id.should eq versions[i].commit_id
      req.versions[i].repo.should eq versions[i].deployable_entity.repo
    end

    # It should use the non-tpm env for non tpm builds
    build = double(
      :build, 
      id: 1, 
      name: "test-build", 
      tpm: false,
      deployable_entity_versions: versions
    )
    req = BuildManager.create_build_request build
    req.build_name.should eq build.name
    req.build_id.should eq build.id
    req.versions.size.should eq versions.size
    versions.each_index do |i|
      req.versions[i].environment.should eq versions[i].deployable_entity.no_tpm_env
      req.versions[i].commit_id.should eq versions[i].commit_id
      req.versions[i].repo.should eq versions[i].deployable_entity.repo
    end
  end
end

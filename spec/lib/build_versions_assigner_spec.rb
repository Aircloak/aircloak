require './lib/build_versions_assigner.rb'

describe BuildVersionsAssigner do
  # If this test is run in isolation, then we 
  # need to mock out BuildVersion and DeployableEntities
  before(:all) do
    begin DeployableEntities
    rescue NameError
      class DeployableEntities
      end
    end

    begin BuildVersion
    rescue NameError
      class BuildVersion
      end
    end
  end

  let(:params) {
    {
      "utf8"=>"✓",
      "authenticity_token"=>"4MnSU5tTng97r+hiQ475eb6621cirgvv58uKQovvTco=",
      "build"=>{"name"=>"asd", "tpm"=>"1"},
      "build_versions"=>["1","3"],
      "commit"=>"Save",
      "action"=>"create",
      "controller"=>"builds"
    }
  }

  it "should assign deployable entities to a build" do
    build = double(:build, id: 2)
    version = double(:version)
    BuildVersion.should_receive(:create).with({
      deployable_entity_version_id: 1,
      build_id: 2
    })

    BuildVersionsAssigner.assign_versions build, [1]
  end

  it "it should add entries, and ensure only one version is added per entity" do
    bv1 = double(:deployable_entity_version1, id: 1)
    bv2 = double(:deployable_entity_version2, id: 2)
    build = double(:build, id: 1, deployable_entity_versions: [bv1, bv2])
    
    BuildVersion.should_receive(:destroy).with(2)

    BuildVersion.should_receive(:create).with({
      deployable_entity_version_id: 3,
      build_id: 1
    })
    
    BuildVersionsAssigner.assign_from_params build, params
  end
end

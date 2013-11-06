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
    
    begin DeployableEntityVersion
    rescue NameError
      class DeployableEntityVersion
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
      "build_versions"=>["1","2"],
      "commit"=>"Save",
      "action"=>"create",
      "controller"=>"builds"
    }
  }

  it "should assign deployable entities to a build" do
    dev = double(:version, id: 1)
    DeployableEntityVersion.should_receive(:find).with(1).and_return(dev)
    devs = []
    build = double(:build, id: 1, deployable_entity_versions: devs)
    BuildVersionsAssigner.assign_versions build, [1]
    devs.size.should eq 1
    devs.should eq [dev]
  end

  it "it should add entries, and ensure only one version is added per entity" do
    bv1 = double(:deployable_entity_version1, id: 1)
    bv2 = double(:deployable_entity_version2, id: 2)

    devs = []
    build = double(:build, id: 1, deployable_entity_versions: devs)

    DeployableEntityVersion.should_receive(:find).with(1).and_return(bv1)
    DeployableEntityVersion.should_receive(:find).with(2).and_return(bv2)
    
    BuildVersionsAssigner.assign_from_params build, params

    devs.size.should eq 2
    devs.should eq [bv1, bv2]
  end
end

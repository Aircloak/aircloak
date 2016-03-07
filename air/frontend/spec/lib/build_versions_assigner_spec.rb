require './lib/build_versions_assigner.rb'

describe BuildVersionsAssigner do
  # If this test is run in isolation, then we
  # need to mock out BuildVersion and DeployableEntity
  before(:all) do
    begin DeployableEntity
    rescue NameError
      class DeployableEntity
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

  let(:branch_selections) {
    [{
      id: 1,
      repo: "de1",
      sha: "abcd"
    },{
      id: 2,
      repo: "de2",
      sha: "efgh"
    }]
  }
  let(:params_from_versions) {
    {
      "utf8"=>"✓",
      "authenticity_token"=>"4MnSU5tTng97r+hiQ475eb6621cirgvv58uKQovvTco=",
      "build"=>{"name"=>"asd"},
      "branch_selections"=>branch_selections.to_json,
      "commit"=>"Save",
      "action"=>"create",
      "controller"=>"builds",
      "from_develop" => "false"
    }
  }
  let(:params_develop) {
    {
      "utf8"=>"✓",
      "authenticity_token"=>"4MnSU5tTng97r+hiQ475eb6621cirgvv58uKQovvTco=",
      "build"=>{"name"=>"asd"},
      "branch_selections"=>branch_selections.to_json,
      "commit"=>"Save",
      "action"=>"create",
      "controller"=>"builds",
      "from_develop" => "true"
    }
  }

  it "it should add entries, and add the right version for each entity" do
    devs = []
    build = double(:build, id: 1, deployable_entity_versions: devs)

    de1 = double(:deployable_entity_1)
    de2 = double(:deployable_entity_2)
    DeployableEntity.should_receive(:find).with(1).and_return(de1)
    DeployableEntity.should_receive(:find).with(2).and_return(de2)
    de1.should_receive(:add_commit).with("abcd")
    de2.should_receive(:add_commit).with("efgh")

    bv1 = double(:deployable_entity_version1, id: 1)
    bv2 = double(:deployable_entity_version2, id: 2)
    DeployableEntityVersion.should_receive(:find_by_commit_id).with("abcd").and_return(bv1)
    DeployableEntityVersion.should_receive(:find_by_commit_id).with("efgh").and_return(bv2)

    BuildVersionsAssigner.assign_from_params build, params_from_versions

    devs.size.should eq 2
    devs.should eq [bv1, bv2]
  end

  it "should not fail when called with empty branch_selections if from_develop is false" do
    build = double(:build)
    params = {
      "from_develop" => "false"
    }
    expect{BuildVersionsAssigner.assign_from_params build, params}.to_not raise_error
  end

  it "should user the most recent versions from develop branch when requested" do
    versions = double()
    entities = double()
    DeployableEntity.should_receive(:all).and_return(entities)
    BuildManager.stub(:find_right_versions).and_return(versions)
    array = []
    build = double(deployable_entity_versions: array)
    BuildVersionsAssigner.assign_from_params build, params_develop
    array.include?(versions).should eq true
  end
end

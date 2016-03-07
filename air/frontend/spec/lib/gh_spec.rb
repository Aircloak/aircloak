require './lib/gh.rb'
require './spec/slim_helpers.rb'

describe Gh do
  before(:each) do
    @msg = "bug fix.\n\nLooking for aikblob in the wrong place (holdover from where\nprivacyca function used to put it)"
    @commit_id = "0253132d55e40e3f8757b4e3dda6d6967fc90726"
  end

  it "should provide a description for a repo name" do
    repo = "testserver"
    desc = "Source code for test orchestration server"
    VCR.use_cassette('github-repo', allow_playback_repeats: true) do
      Gh.description_for(repo).should eq(desc)
    end
  end

  it "should throw an error on repos that do not exist" do
    VCR.use_cassette('github-missing-repo', allow_playback_repeats: true) do
      expect { Gh.description_for("bogus_repo") }.to raise_error UnknownRepository
    end
  end

  it "should be able to set the commit message and author on a version" do
    entity = double(:entity, repo: "erlattest")
    version = double(:version, commit_id: @commit_id, deployable_entity: entity)
    allow(version).to receive(:message=)
    allow(version).to receive(:author=)
   
    VCR.use_cassette('erlattest-commit-message', allow_playback_repeats: true) do
      Gh.add_message_and_author version
    end

    expect(version).to have_received(:message=).with(@msg)
    expect(version).to have_received(:author=).with("root")
  end

  it "should be able to provide the last commit id for a repo" do
    VCR.use_cassette('erlattest-commits-on-develop', allow_playback_repeats: true) do
      Gh.latest_commit_on_branch_for_repo("develop", "erlattest").should eq "0fe016e83797498f4fc0400aff71c0707076b901"
    end
  end
end

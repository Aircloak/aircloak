require './lib/gh.rb'
require './spec/slim_helpers.rb'

describe Gh do
  it "should provide a description for a repo name" do
    repo = "testserver"
    desc = "Source code for test orchestration server"
    VCR.use_cassette('github-repo') do
      Gh.description_for(repo).should eq(desc)
    end
  end

  it "should throw an error on repos that do not exist" do
    VCR.use_cassette('github-missing-repo') do
      expect { Gh.description_for("bogus_repo") }.to raise_error UnknownRepository
    end
  end
end

require 'spec_helper'

describe Analyst do
  before(:each) do
    Analyst.destroy_all
  end

  let(:analyst) { Analyst.create name: "test analyst" }

  it "should have a name" do
    Analyst.create.errors.should include(:name)
  end

  it "should have a key" do
    Analyst.create(name: "test").key.should_not eq nil
  end

  it "should know if it has clusters or not" do
    analyst.has_clusters?.should eq false
    analyst.should_receive(:clusters).and_return([double(:cluster)])
    analyst.has_clusters?.should eq true
  end
end

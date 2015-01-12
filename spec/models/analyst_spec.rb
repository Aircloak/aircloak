require 'spec_helper'

describe Analyst do
  before(:each) do
    Analyst.destroy_all
    RepeatedAnswer.delete_all
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

  it "should remove repeated answers when being removed" do
    analyst.repeated_answers << RepeatedAnswer.new(
      bucket_label: "label",
      bucket_count: 1,
      timestamp: Time.now.to_i,
      source_ip: "127.0.0.1",
      noise_sd: 2.2
    )
    analyst.save
    RepeatedAnswer.count.should eq 1
    analyst.destroy
    RepeatedAnswer.count.should eq 0
  end
end

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

  it "creates and revokes key materials" do
    KeyMaterial.should_receive(:api_ca).and_return(TokenGenerator.generate_root_token("air_web_api", -1))
    ["data_upload_all", "admin", "task_runner", "web_api"].each do |key_type|
      km = KeyMaterial.create_from_analyst analyst, "foobar", "desc", key_type
      km.analyst.should eq analyst
      km.description.should eq "desc"
      km.key_type.should eq key_type
      km.revoked.should eq false
      token = nil
      if key_type == "web_api"
        km.analyst_token.should_not eq nil
        token = km.analyst_token
      else
        km.analyst_token.should eq nil
      end

      analyst.revoke_key(km)
      km.revoked.should eq true
      km.analyst_token.should eq nil
      if token
        AnalystToken.find_by_id(token.id).should eq nil
        AnalystToken.find_by_token(token.token).should eq nil
      end
    end
  end
end

require 'spec_helper'
# require './lib/proto/air/aggregate_results.pb.rb'

describe PropertyResult do
  it "should create results from proto" do
    prop1 = PropertyProto.new name: "testProto", str_answer: "testAnswer", count: 1
    prop2 = PropertyProto.new name: "testProto", str_answer: "testAnswer", count: 1
    query_id = 1

    Property.where(query_id: query_id).first.should == nil
  end
end

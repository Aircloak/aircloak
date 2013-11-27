require './lib/result_handler'
require './lib/proto/air/aggregate_results.pb'

describe ResultHandler do
  before(:each) do
    class Result; end
    class Bucket; end
  end

  it "should create a bucket from a PropertyProto (without range)" do
    jl = JoinersLeaversProto.new(joiners: 73, leavers: 37)
    property = PropertyProto.new(label: "label", string: "string", joiners_leavers: jl, accumulated_count: 32)
    Bucket.should_receive(:create).with(label: "label", str_answer: "string", range_min: nil, range_max: nil,
        joiners: 73, leavers: 37, accumulated_count: 32)
    ResultHandler.create_bucket_for_property property
  end

  it "should create a bucket from a PropertyProto (with range)" do
    jl = JoinersLeaversProto.new(joiners: 37, leavers: 73)
    r = PropertyProto::RangeProto.new(min: -12, max: 33)
    property = PropertyProto.new(label: "label", range: r, joiners_leavers: jl, accumulated_count: 23)
    Bucket.should_receive(:create).with(label: "label", str_answer: nil, range_min: -12, range_max: 33,
        joiners: 37, leavers: 73, accumulated_count: 23)
    ResultHandler.create_bucket_for_property property
  end

  it "should create a new result from a ResultProto" do
    task = double
    new_result = double
    new_result.stub(:save)
    result = ResultProto.new(analyst_id: "analyst", task_id: 10, index: "index", result_id: 12,
        properties: [], exceptions: [])
    Result.should_receive(:new).with(query: task, result_id: 12).and_return(new_result)
    ResultHandler.store_results task, result
  end

  it "should create all buckets from a ResultProto" do
    jl = JoinersLeaversProto.new(joiners: 73, leavers: 37)
    property1 = PropertyProto.new(label: "label1", joiners_leavers: jl, accumulated_count: 32)
    property2 = PropertyProto.new(label: "label2", joiners_leavers: jl, accumulated_count: 32)
    bucket1 = double
    bucket2 = double
    task = double
    buckets = double
    new_result = double(buckets: buckets)
    new_result.stub(:save)
    ResultHandler.should_receive(:create_bucket_for_property).with(property1).and_return(bucket1)
    ResultHandler.should_receive(:create_bucket_for_property).with(property2).and_return(bucket2)
    buckets.should_receive(:<<).with(bucket1)
    buckets.should_receive(:<<).with(bucket2)
    result = ResultProto.new(analyst_id: "analyst", task_id: 10, index: "index", result_id: 12,
        properties: [property1, property2], exceptions: [])
    Result.should_receive(:new).with(query: task, result_id: 12).and_return(new_result)
    ResultHandler.store_results task, result
  end
end

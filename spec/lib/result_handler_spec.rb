require './lib/result_handler'

describe ResultHandler do
  def buckets post_processings=[]
    buckets = [
      {"label"=>"some_label", "value"=>"some_value", "count"=>1000}
    ]
    post_processings.each do |type|
      buckets.push({"label"=>"ac_postprocessing", "value"=>type, "count"=>1000})
    end
    buckets
  end

  context "determining post processing type" do
    it "should know when a task does not need post processing" do
      ResultHandler.post_processing_types(buckets).should eq []
    end
    it "should detect valid post processing types" do
      ResultHandler.post_processing_types(buckets(["ecdf"])).should eq ["ecdf"]
      ResultHandler.post_processing_types(buckets(["ecdf", "histogram"])).should eq ["ecdf", "histogram"]
    end
    it "should ignore unknown post processing types" do
      ResultHandler.post_processing_types(buckets(["unsupported"])).should eq []
    end
  end

  context "post processing" do
    it "should return results unchanged if no post-processing is requested" do
      ResultHandler.post_process_buckets(buckets).should eq buckets
    end
  end
end

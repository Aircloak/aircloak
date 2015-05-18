require './lib/post_processors/ecdf'

describe ECDF do
  it "should invert a CDF" do
    # We create a straight line bucket input list, that can be converted
    # into a straight line CDF for a simplistic test.
    buckets = [{"label" => "ac_postprocessing", "value" => "ecdf", "count" => 100}]
    100.downto(0) do |num|
      buckets << {"label"=>"ac_ecdf_val_less_or_equal", "value"=>"#{100-num}", "count"=>num}
    end

    results = ECDF.process(buckets)
    results.size.should eq buckets.size
    results.each do |result|
      if result["label"] == "ac_query" then
        result["value"].should eq "ecdf"
      else
        result["value"].to_i.should eq result["count"]
      end
    end
  end
end

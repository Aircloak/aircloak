require './lib/post_processors/ecdf'
require 'pry'

describe ECDF do
  def bucket label, value
    {"label" => label, "value" => value, "count" => 100}
  end

  it "should invert a CDF" do
    # We create a straight line bucket input list, that can be converted
    # into a straight line CDF for a simplistic test.

    buckets = [
      bucket("ac_postprocessing", "ecdf"),
      bucket("ac_ecdf_x_label", "x_label"),
      bucket("ac_ecdf_y_label", "y_label"),
      bucket("ac_ecdf_legend", "legend"),
      bucket("ac_ecdf_title", "title")
    ]
    100.downto(0) do |num|
      buckets << {"label"=>"ac_ecdf_val_less_or_equal", "value"=>"#{100-num}", "count"=>num}
    end

    results = ECDF.process(buckets)
    results.size.should eq 1
    result = results.first
    result["label"].should eq "ac_graph"
    value = JSON.parse(result["value"])

    value["type"].should eq "ecdf"
    ["x_label", "y_label", "legend", "title"].each do |item|
      value[item].should eq item
    end
    value["data"].each do |v|
      v[0].should eq v[1]
    end
  end
end

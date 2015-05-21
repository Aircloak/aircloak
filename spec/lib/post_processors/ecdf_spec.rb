require './lib/post_processors/ecdf'
require 'pry'

describe ECDF do
  def bucket label, value
    {"label" => label, "value" => value, "count" => 100}
  end

  def base_buckets
    [
      bucket("ac_postprocessing", "ecdf"),
      bucket("ac_ecdf_x_label", "x_label"),
      bucket("ac_ecdf_y_label", "y_label"),
      bucket("ac_ecdf_legend", "legend"),
      bucket("ac_ecdf_title", "title")
    ]
  end

  def cdf_value value, count
    {"label" => "ac_ecdf_val_less_or_equal", "value" => value.to_s, "count" => count}
  end

  it "should invert a CDF" do
    # We create a straight line bucket input list, that can be converted
    # into a straight line CDF for a simplistic test.
    buckets = base_buckets
    100.downto(0) {|num| buckets << cdf_value(100-num, num)}

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

  it "should produce an aesthetically pleasing CDF with ever increasing values" do
    # We create a straight line bucket input list, that can be converted
    # into a straight line CDF for a simplistic test.
    buckets = base_buckets
    buckets << cdf_value(1, 100)
    buckets << cdf_value(2, 50)
    buckets << cdf_value(3, 75)
    buckets << cdf_value(4, 0)

    results = ECDF.process(buckets)
    result = results.first
    value = JSON.parse(result["value"])
    data = value["data"].sort_by {|v| v["x"]}
    data.inject(0) {|min,v| y = v["y"]; (min <= y).should eq true; y}
  end
end

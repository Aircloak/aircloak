require './lib/post_processors/ecdf'

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
    buckets << cdf_value(4, 76.3)
    buckets << cdf_value(5, 0)

    results = ECDF.process(buckets)
    result = results.first
    value = JSON.parse(result["value"])
    data = value["data"].sort_by {|v| v["x"]}
    data.inject(0) {|min,v| y = v["y"]; (min <= y).should eq true; y}

    # -----

    buckets = base_buckets
    buckets << cdf_value(1, 1)
    buckets << cdf_value(2, 2)
    buckets << cdf_value(3, 3)
    buckets << cdf_value(4, 4)

    results = ECDF.process(buckets)
    result = results.first
    value = JSON.parse(result["value"])
    data = value["data"].sort_by {|v| v["x"]}
    data.map! {|v| v["y"]}
    data.should eq [100, 100, 100, 100]

    # -----

    buckets = base_buckets
    buckets << cdf_value(1, 2)
    buckets << cdf_value(2, 1)
    buckets << cdf_value(3, 0)
    buckets << cdf_value(4, 1)
    buckets << cdf_value(5, 2)

    results = ECDF.process(buckets)
    result = results.first
    value = JSON.parse(result["value"])
    data = value["data"].sort_by {|v| v["x"]}
    data.map! {|v| v["y"].to_i}
    data.should eq [0, 66, 100, 100, 100]
  end

  it "should cut of long tails of 0%" do
    buckets = base_buckets
    buckets << cdf_value(1, 100)
    buckets << cdf_value(2, 100)
    buckets << cdf_value(3, 100)
    buckets << cdf_value(4, 50)
    buckets << cdf_value(5, 0)

    results = ECDF.process(buckets)
    result = results.first
    value = JSON.parse(result["value"])
    data = value["data"].sort_by {|v| v["x"]}
    y_axis = data.map {|v| v["y"].to_i}
    y_axis.should eq [0, 50, 100]
    x_axis = data.map {|v| v["x"]}
    first_x_axis = x_axis.shift
    # We want to make sure only the last 0 is included
    x_axis.inject(first_x_axis) {|prev,curr| (prev + 1).should eq curr; curr}
  end
end

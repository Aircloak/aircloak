require './lib/result_handler'

# We conditionally construct the
# Property class so the ResultHandler
# can be tested with spec/test
begin Property
rescue NameError
  class Property
  end
end

describe ResultHandler do
  before do
    @query_id = 1
    @index = "index"
    @property_proto = double("property_proto", query_id: @query_id, label: "installed_apps", string: "Chrome", range: nil, joiners_leavers: double("joiners_leavers_proto", joiners: 2, leavers: 1))
    @range = double("range_proto", min: 1, max: 10)
    @property_proto_range = double("property_proto", query_id: 1, label: "installed_apps_count", string: nil, range: @range, joiners_leavers: double("joiners_leavers_proto", joiners: 3, leavers: 4))
    @property = double("property", query_id: 1, id: 2, property: "installed_apps")
    @property_result = double("property_result", property_id: 2, str_value: "Chrome")
    @property_result_range = double("property_result", property_id: 2, str_value: nil, has_range: true, range_min: 1, range_max: 10)
  end

  it "should load existing properties if they exist" do
    Property.should_receive(:where).with(query_id: 1, index: @index, property: "installed_apps").and_return([@property_proto])
    p = ResultHandler.get_property! @query_id, @index, double("property", label: "installed_apps")
    p.should_not eq(nil)
  end

  it "should create properties if they do not exist" do
    Property.should_receive(:where).with(query_id: 1, index: @index, property: "installed_apps").and_return([])
    Property.should_receive(:create).with(query_id: 1, index: @index, property: "installed_apps").and_return(@property_proto)
    p = ResultHandler.get_property! @query_id, @index, @property_proto
    p.should_not eq(nil)
  end

  it "should load existing property results if they exist" do
    r = double("relation")
    r.should_receive(:where).and_return([@property_result], [@property_result_range])
    @property.stub(property_results: r)
    ResultHandler.get_property_result!(@property, @property_proto).should eq(@property_result)
    ResultHandler.get_property_result!(@property, @property_proto_range).should eq(@property_result_range)
  end

  it "should create new property results if no old one exists" do
    r = double("relation", where: [])
    @property.stub(property_results: r)

    r.should_receive(:create).with(str_value: "Chrome", has_range: false).and_return(@property_result)
    r.should_receive(:create).with(str_value: nil, has_range: true, range_min: 1, range_max: 10).and_return(@property_result_range)

    ResultHandler.get_property_result!(@property, @property_proto).should eq(@property_result)
    ResultHandler.get_property_result!(@property, @property_proto_range).should eq(@property_result_range)
  end

  it "should add counts to property results" do
    ResultHandler.should_receive(:'get_property!').with(@query_id, @index, @property_proto).and_return(@property)
    ResultHandler.should_receive(:'get_property_result!').with(@property, @property_proto).and_return(@property_result)
    relation = double("relation")
    @property_result.should_receive(:property_result_counts).and_return(relation)
    relation.should_receive(:create).with(joiners: 2, leavers: 1).and_return(double("prc"))
    ResultHandler.add_property_result(@query_id, @index, @property_proto).should eq(true)
  end
end

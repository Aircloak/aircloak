require './lib/result_handler'

describe ResultHandler do
  before do
    @query_id = 1
    @property_proto = double("property_proto", query_id: @query_id, name: "installed_apps", str_answer: "Chrome", long_answer: nil, count: 1)
    @property_proto_numeric = double("property_proto", query_id: 1, name: "installed_apps_count", str_answer: nil, long_answer: 2, count: 1)
    @property = double("property", query_id: 1, id: 2, property: "installed_apps")
    @property_result = double("property_result", property_id: 2, str_value: "Chrome")
    @property_result_numeric = double("property_result", property_id: 2, str_value: nil, long_value: 2)
  end

  it "should load existing properties if they exist" do
    Property.should_receive(:where).with(query_id: 1, property: "installed_apps").and_return([@property_proto])
    p = ResultHandler.get_property! @query_id, double("property", name: "installed_apps")
    p.should_not eq(nil)
  end

  it "should create properties if they do not exist" do
    Property.should_receive(:where).with(query_id: 1, property: "installed_apps").and_return([])
    Property.should_receive(:create).with(query_id: 1, property: "installed_apps").and_return(@property_proto)
    p = ResultHandler.get_property! @query_id, @property_proto
    p.should_not eq(nil)
  end

  it "should know when a property is numeric" do
    ResultHandler.is_numeric?(double(long_answer: nil, str_answer: "foo")).should eq(false)
    ResultHandler.is_numeric?(double(long_answer: 2, str_answer: nil)).should eq(true)
    ResultHandler.is_numeric?(@property_proto).should eq(false)
    ResultHandler.is_numeric?(@property_proto_numeric).should eq(true)
  end

  it "should load existing property results if they exist" do
    r = double("relation")
    r.should_receive(:where).and_return([@property_result], [@property_result_numeric])
    @property.stub(property_results: r)
    ResultHandler.get_property_result!(@property, @property_proto).should eq(@property_result)
    ResultHandler.get_property_result!(@property, @property_proto_numeric).should eq(@property_result_numeric)
  end

  it "should create new property results if no old one exists" do
    r = double("relation", where: [])
    @property.stub(property_results: r)

    r.should_receive(:create).with(numeric: false, str_value: "Chrome").and_return(@property_result)
    r.should_receive(:create).with(numeric: true, long_value: 2).and_return(@property_result_numeric)

    ResultHandler.get_property_result!(@property, @property_proto).should eq(@property_result)
    ResultHandler.get_property_result!(@property, @property_proto_numeric).should eq(@property_result_numeric)
  end

  it "should add counts to property results" do
    ResultHandler.should_receive(:'get_property!').with(@query_id, @property_proto).and_return(@property)
    ResultHandler.should_receive(:'get_property_result!').with(@property, @property_proto).and_return(@property_result)
    relation = double("relation")
    @property_result.should_receive(:property_result_counts).and_return(relation)
    relation.should_receive(:create).with(count: 1).and_return(double("prc"))
    ResultHandler.add_property_result(@query_id, @property_proto).should eq(true)
  end
end

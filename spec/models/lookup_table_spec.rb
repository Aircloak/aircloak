require 'spec_helper'

describe LookupTable do
  before(:each) do
    LookupTable.destroy_all
  end

  it "should validate presence" do
    LookupTable.create.errors.should include(:table_name, :cluster, :analyst, :upload_data)
  end

  it "validates upload data" do
    table_template('invalid json').errors[:upload_data].should eq ["must be in json format"]
    table_template('{}').errors[:upload_data].should eq ["json is not valid"]
    table_template('[["key"]]').errors[:upload_data].should eq ["json is not valid"]
    table_template('[["key", []]]').errors[:upload_data].should eq ["json is not valid"]
    table_template('[["key", "value", "value2"]]').errors[:upload_data].should eq ["json is not valid"]
  end

  it "accepts correct data" do
    table_template('[]').errors.count.should eq 0
    table_template('[["k1", "v1"], ["k2", "v2"]]').errors.count.should eq 0
  end

private
  def table_template(upload_data)
    table = LookupTable.new
    table.stub(:cluster) {Cluster.new(id: 1)}
    table.stub(:analyst) {Analyst.new(id: 1)}
    table.table_name = "foo"
    table.upload_data = double(read: upload_data)
    table.valid?
    table
  end
end

require 'spec_helper'

describe LookupTable do
  before(:each) do
    ProtobufSender.stub(:send_delete)
    LookupTable.destroy_all
    Analyst.destroy_all
    Cluster.destroy_all
  end

  let (:cluster) { Cluster.create id: 1, name: "test-cluster" }
  let (:analyst) { Analyst.create id: 1, name: "test-analyst" }

  it "should validate presence" do
    LookupTable.create.errors.should include(:table_name, :cluster, :analyst, :upload_data)
  end

  it "validates upload data" do
    table_template('foo', 'invalid json').errors[:upload_data].should eq ["must be in json format"]
    table_template('foo', '{}').errors[:upload_data].should eq ["json is not valid"]
    table_template('foo', '[["key"]]').errors[:upload_data].should eq ["json is not valid"]
    table_template('foo', '[["key", []]]').errors[:upload_data].should eq ["json is not valid"]
    table_template('foo', '[["key", "value", "value2"]]').errors[:upload_data].should eq ["json is not valid"]
  end

  it "accepts correct data" do
    table_template('foo', '[]').valid?.should eq true
    table_template('foo', '[["k1", "v1"], ["k2", "v2"]]').valid?.should eq true
  end

  it "identifies duplicate table" do
    table_template('foo', '[]').save.should eq true
    table_template('foo', '[]').errors[:table_name].should eq ["must be unique"]
  end

  it "allows duplicate names across clusters" do
    table_template('foo', '[]').save.should eq true
    t = table_template('foo', '[]')
    t.valid?.should eq false
    t.cluster = Cluster.create(id: 2, name: "test-cluster")
    t.valid?.should eq true
  end

private
  def table_template(table_name, upload_data)
    table = LookupTable.new(
          cluster: cluster,
          analyst: analyst,
          table_name: table_name,
          upload_data: double(read: upload_data)
        )
    table.valid?
    table
  end
end

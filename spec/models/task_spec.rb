require 'spec_helper'
require 'prefetch_helper'

describe Task do
  include PrefetchHelper

  it "should have required attributes" do
    required_fields = [:name, :sandbox_type, :code, :cluster, :data]
    task1 = Task.new(required_fields.inject({}) {|memo, field| memo.merge(field => nil)})
    task1.save.should eq false

    required_fields.each do |field|
      fail "not verifying presence of field #{field}" unless task1.errors[field][0] == "can't be blank"
    end
  end

  it "validates prefetch" do
    try_invalid_save(base_attrs.merge(data: "[]"))[:data].should eq ["can't be blank"]
  end

  it "converts data" do
    prefetch_conversions.each do |data, prefetch|
      task = Task.new
      task.stub(:analyst) {analyst_double([age_table_double])}
      task.attributes = base_attrs.merge(data: data)
      task.prefetch.should eq prefetch

      AnalystTable.should_receive(:where).with(table_name: "age").and_return([age_table_double])
      Task.new(base_attrs.merge(prefetch: prefetch)).data.should eq data
    end
  end

  it "returns correct result_set" do
    task = Task.new

    task.stub(:results) do
      [
        double(id: 1, buckets: [
          double(display_name: "bucket2", display_result: "res12", result_id: 1),
          double(display_name: "bucket1", display_result: "res11", result_id: 1)
        ]),

        double(id: 2, buckets: [
          double(display_name: "bucket3", display_result: "res23", result_id: 2),
          double(display_name: "bucket1", display_result: "res21", result_id: 2)
        ])
      ]
    end

    task.result_set.
        should eq({"bucket1"=>{1=>"res11", 2=>"res21"}, "bucket2"=>{1=>"res12"}, "bucket3"=>{2=>"res23"}})
  end

  private
    def base_attrs
      {name: "name", sandbox_type: "lua", code: "code", cluster: Cluster.new}
    end

    def try_invalid_save(task_spec)
      task = Task.new(task_spec)
      task.save.should eq false
      task.errors
    end
end

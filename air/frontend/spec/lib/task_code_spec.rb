require './lib/task_code.rb'
require './spec/slim_helpers.rb'

describe TaskCode do
  before(:all) do
    begin TaskLibrary
    rescue NameError
      class TaskLibrary
      end
    end
  end

  it "resolves dependencies in proper order" do
    TaskLibrary.should_receive(:all).and_return([
      double(name: "Aircloak", code: "ac"),
      double(name: "Aircloak.Date", code: "acd"),
      double(name: "Aircloak.DateTime", code: "Aircloak.Date.parse_date(20150101)"),
      double(name: "Aircloak.Math", code: "acm"),
      double(name: "Aircloak.Constants", code: "acc")
    ])

    TaskCode.dependencies("""
          foo

          Aircloak.Math.quantize(25)
          Aircloak.DateTime.to_date(20150101)
          Aircloak.Constants.PI

          bar
        """).should eq([
              {:name=>"Aircloak", :code=>"ac"},
              {:name=>"Aircloak.Math", :code=>"acm"},
              {:name=>"Aircloak.Date", :code=>"acd"},
              {:name=>"Aircloak.DateTime", :code=>"Aircloak.Date.parse_date(20150101)"},
              {:name=>"Aircloak.Constants", :code=>"acc"}
            ])
  end

  it "ignores unknown namespaces" do
    TaskLibrary.should_receive(:all).and_return([
      double(name: "Aircloak.Date", code: ""),
      double(name: "Aircloak.Constants", code: "")
    ])

    TaskCode.dependencies("""
          foo

          Aircloak.Math.quantize(25)
          Aircloak.DateTime.to_date(20150101)
          Aircloak.Constants.PI

          bar
        """).map{|x| x[:name]}.should eq(["Aircloak.Constants"])
  end
end
require 'spec_helper'

describe "ApiTestResultsController" do
  describe "POST /infrastructure-api/test_results" do
    before(:each) do
      TestResult.delete_all
      TestItem.delete_all
      TestItemVm.delete_all
      TestVm.delete_all
    end

    let (:request) {
      {
        timestamp: 1234,
        test_server_version: "SHA-1",
        duration: 232,
        benchmark: {
          success: true,
          duration: 2,
          coverage: 0.1,
          memory_usage: 200
        },
        vms: [
          {name: "foo", duration: 1, disk_size: 100, disk_usage: 10, log: "foo", success: true},
          {name: "bar", duration: 2, disk_size: 100, disk_usage: 20},
          {name: "failed vm", duration: 2, disk_size: 100, disk_usage: 20, success: false}
        ],
        tests: [
          {
            name: "foobar",
            success: true,
            duration: 23,
            log: "flarg"
          },
          {
            name: "foobaz",
            success: true,
            duration: 23,
            vms: [
              {
                name: "foo-inst",
                type: "foo",
                cpus: 13,
                memory_size: 1024,
                memory_usage: 10,
                disk_usage: 99
              }
            ]
          },
          {
            name: "failed test 2",
            success: false,
            duration: 19,
            log: "I failed too"
          },
          {
            name: "failed test 1",
            success: false,
            duration: 20,
            log: "I failed"
          }
        ]
      }
    }

    it "should take a valid request" do
      post "/infrastructure-api/test_results", request.to_json
      response.status.should eq(200)
    end

    it "requires a JSON" do
      post "/infrastructure-api/test_results", "foo"
      response.status.should eq(403)
    end

    it "requires .timestamp" do
      request.delete(:timestamp)
      post "/infrastructure-api/test_results", request.to_json
      response.status.should eq(403)
    end

    it "requires .test_server_version" do
      request.delete(:test_server_version)
      post "/infrastructure-api/test_results", request.to_json
      response.status.should eq(403)
    end

    it "requires .duration" do
      request.delete(:duration)
      post "/infrastructure-api/test_results", request.to_json
      response.status.should eq(403)
    end

    it "requires .benchmark.duration" do
      request[:benchmark].delete(:duration)
      post "/infrastructure-api/test_results", request.to_json
      response.status.should eq(403)
    end

    it "requires .benchmark.coverage" do
      request[:benchmark].delete(:coverage)
      post "/infrastructure-api/test_results", request.to_json
      response.status.should eq(403)
    end

    it "requires .benchmark.memory_usage" do
      request[:benchmark].delete(:memory_usage)
      post "/infrastructure-api/test_results", request.to_json
      response.status.should eq(403)
    end

    it "requires .vms[].name" do
      request[:vms][1].delete(:name)
      post "/infrastructure-api/test_results", request.to_json
      response.status.should eq(403)
    end

    it "requires .vms[].duration" do
      request[:vms][1].delete(:duration)
      post "/infrastructure-api/test_results", request.to_json
      response.status.should eq(403)
    end

    it "requires .vms[].disk_size" do
      request[:vms][1].delete(:disk_size)
      post "/infrastructure-api/test_results", request.to_json
      response.status.should eq(403)
    end

    it "requires .vms[].disk_usage" do
      request[:vms][1].delete(:disk_usage)
      post "/infrastructure-api/test_results", request.to_json
      response.status.should eq(403)
    end

    it "requires .tests[].name" do
      request[:tests][1].delete(:name)
      post "/infrastructure-api/test_results", request.to_json
      response.status.should eq(403)
    end

    it "requires .tests[].duration" do
      request[:tests][1].delete(:duration)
      post "/infrastructure-api/test_results", request.to_json
      response.status.should eq(403)
    end

    it "requires .tests[].vms[].name" do
      request[:tests][1][:vms][0].delete(:name)
      post "/infrastructure-api/test_results", request.to_json
      response.status.should eq(403)
    end

    it "requires .tests[].vms[].cpus" do
      request[:tests][1][:vms][0].delete(:cpus)
      post "/infrastructure-api/test_results", request.to_json
      response.status.should eq(403)
    end

    it "requires .tests[].vms[].memory_size" do
      request[:tests][1][:vms][0].delete(:memory_size)
      post "/infrastructure-api/test_results", request.to_json
      response.status.should eq(403)
    end

    it "requires .tests[].vms[].memory_usage" do
      request[:tests][1][:vms][0].delete(:memory_usage)
      post "/infrastructure-api/test_results", request.to_json
      response.status.should eq(403)
    end

    it "requires .tests[].vms[].disk_usage" do
      request[:tests][1][:vms][0].delete(:disk_usage)
      post "/infrastructure-api/test_results", request.to_json
      response.status.should eq(403)
    end
  end
end

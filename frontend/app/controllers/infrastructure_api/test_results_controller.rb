class InfrastructureApi::TestResultsController < ApplicationController
  protect_from_forgery :except => :create

  def create
    raw_result = JSON.parse request.raw_post
    InfrastructureApi::TestResultsController.create_test_result raw_result
    render json: {success: true}, status: 200
  rescue Exception => e
    render json: {success: false, error: e.to_s}, status: 403
  end

private

  def self.create_test_result raw_result
    has_failure = false
    testResult = TestResult.new(
      testtime: raw_result["timestamp"],
      test_server_version: raw_result["test_server_version"],
      duration: raw_result["duration"],
      benchmark_success: raw_result["benchmark"]["success"],
      benchmark_duration: raw_result["benchmark"]["duration"],
      benchmark_coverage: raw_result["benchmark"]["coverage"],
      benchmark_memory_usage: raw_result["benchmark"]["memory_usage"]
    )
    vms = if raw_result["vms"]
      raw_result["vms"].inject({}) do |vms, raw_vm|
        has_failure = true if raw_vm["success"] == false
        testVm = TestVm.new(
          name: raw_vm["name"],
          success: raw_vm["success"],
          duration: raw_vm["duration"],
          disk_size: raw_vm["disk_size"],
          disk_usage: raw_vm["disk_usage"],
          log: raw_vm["log"]
        )
        testResult.test_vms << testVm
        vms[raw_vm["name"]] = testVm
        vms
      end
    else
      {}
    end
    if raw_result["tests"]
      raw_result["tests"].each do |raw_test|
        has_failure = true if raw_test["success"] == false
        testItem = TestItem.new(
          name: raw_test["name"],
          success: raw_test["success"],
          duration: raw_test["duration"],
          log: raw_test["log"]
        )
        testResult.test_items << testItem
        if raw_test["vms"]
          raw_test["vms"].each do |raw_item_vm|
            testItemVm = TestItemVm.new(
              name: raw_item_vm["name"],
              test_vm: vms[raw_item_vm["type"]],
              cpus: raw_item_vm["cpus"],
              memory_size: raw_item_vm["memory_size"],
              memory_usage: raw_item_vm["memory_usage"],
              disk_usage: raw_item_vm["disk_usage"],
              log: raw_item_vm["log"]
            )
            testItem.test_item_vms << testItemVm
          end
        end
      end
    end
    could_save = testResult.save
    TestMailer.test_failed(testResult).deliver if (has_failure and could_save)
    raise "cannot save" unless could_save
  end

end

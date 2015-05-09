require 'spec_helper'

describe TestResult do
  before(:each) do
    TestResult.delete_all
    TestVm.delete_all
    TestItem.delete_all
  end

  let(:test_result) {
    TestResult.new(
      benchmark_success: true,
      testtime: Time.now,
      test_server_version: "version",
      duration: 1,
      benchmark_duration: 1,
      benchmark_coverage: 100,
      benchmark_memory_usage: 1
    )
  }

  def vm params = {}
    TestVm.new(
      success: params.delete(:success),
      name: "test vm name",
      duration: 1,
      disk_size: 1,
      disk_usage: 100
    )
  end

  def test params = {}
    TestItem.new(
      duration: 1,
      name: "test item name",
      success: params.delete(:success) || false
    )
  end

  it "should fail if benchmark success if false" do
    test_result.success.should eq true
    test_result.benchmark_success = false
    test_result.success.should eq false
  end

  it "should fail if not all vms succeeded" do
    test_result.test_vms << vm(success: true)
    test_result.success.should eq true
    test_result.test_vms << vm(sucess: false)
    test_result.success.should eq false
  end

  it "should fail if not all tests succeeded" do
    test_result.test_items << test(success: true)
    test_result.success.should eq true
    test_result.test_items << test(success: false)
    test_result.success.should eq false
  end

  it "should return failed tests" do
    test_result.test_items << test(success: true)
    test_result.save.should eq true
    test_result.failed_tests.should eq []
    test_result.test_items << test(success: false)
    test_result.save.should eq true
    test_result.failed_tests.count.should eq 1
    test_result.failed_tests.first.success.should eq false
  end

  it "should return failed vms" do
    test_result.test_vms << vm(success: true)
    test_result.save.should eq true
    test_result.failed_vms.should eq []
    test_result.test_vms << vm(success: false)
    test_result.save.should eq true
    test_result.failed_vms.count.should eq 1
    test_result.failed_vms.first.success.should eq false
  end
end

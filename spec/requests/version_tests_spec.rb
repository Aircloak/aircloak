require 'spec_helper'
require './lib/proto/air/version_test_messages.pb'

describe "VersionTestController" do
  describe "PUT /version_tests/ID" do
    it "should fail gracefully if the test is missing" do
      VersionTest.should_receive(:find).and_raise(ActiveRecord::RecordNotFound)
      tr = TestResponsePB.new(
        id: 1,
        success: true,
        transcript: "First this then that"
      )
      put '/api/version_tests/1', tr.encode.buf
      response.status.should eq(404)
    end

    it "should update the test if it exists" do
      tr = TestResponsePB.new(
        id: 1,
        success: true,
        transcript: "First this then that"
      )
      version_test = double
      VersionTest.should_receive(:find).and_return(version_test)
      version_test.should_receive(:process_result).with(tr)
      put '/api/version_tests/1', tr.encode.buf
      response.status.should eq(200)
    end
  end
end

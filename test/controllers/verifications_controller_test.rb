require 'test_helper'

class VerificationsControllerTest < ActionController::TestCase
  test "should reject events with missing pieces of information" do
    datapoints.each do |part|
      resp = resp_for_event data_minus part
      assert_equal "error", resp["status"], "Should fail if not data"
      assert resp["reason"] =~ /#{part}/i, "Should complain about no #{part}, instead: #{resp['reason']}"
    end
  end

  test "should create an event for valid event data" do
    resp = resp_for_event data
    assert_response :success
    assert_equal "ok", resp["status"]
    file_version = ClientFileVersion.where(sha1: data[:version]).first
    assert_not_nil file_version
    event = file_version.client_file_events.first
    assert_not_nil event
    [:event, :description, :positive].each do |part|
      assert_equal data[part], event.send(part), "Expected correct value for #{part}"
    end
  end

private
  def datapoints
    [:machine, :version, :event, :positive, :description]
  end

  def data_minus what
    d = data
    d.delete(what)
    d
  end

  def data
    {
      machine: "test-machine",
      event: "Event",
      positive: true,
      description: "Description",
      version: "version-sha"
    }
  end

  def resp_for_event data
    raw_resp = post :event, data.to_json
    JSON.parse(raw_resp.body)
  end
end

require './lib/protobuf_sender'
require './lib/build_manager'
require './lib/proto/air/version_test_messages.pb'

class VersionTest < ActiveRecord::Base
  belongs_to :build
  belongs_to :cluster
  belongs_to :deployable_entity_version

  def self.new_from_deployable_entity_version version
    v = VersionTest.new deployable_entity_version_id: version.id
    v.test_complete = false
    v.build = BuildManager.test_build_for_version version
    v.save
  end

  # Marking the build as complete has the side effect of creating
  # a cluster for the particular build
  def mark_build_as_complete
    self.cluster = Cluster.test_cluster_for_build build
    self
  rescue NotEnoughCloaks
    set_failed
  end

  def mark_build_as_failed
    set_failed
  end

  def mark_cluster_as_ready
    ProtobufSender.post_to_url test_server_post_url, as_test_request
  end

  def as_test_request
    TestRequestPB.new(
      id: id,
      cluster_id: cluster.id,
      reply_host: "http://#{Rails.configuration.web.host}",
      cluster_nodes: cluster.cloaks.map(&:name)
    )
  end

  # Handles responses from the test server
  def process_result result
    self.test_complete = true
    self.test_success = result.success
    self.test_output = result.transcript
    save
  end

  def status
    return "In progress" unless test_complete
    # The following is assumed:
    # - test_success is always set when test_complete is set
    test_complete && test_success ? "Passed" : "Failed"
  end

private
  def set_failed
    self.test_complete = true
    self.test_success = false
    save
  end

  def test_server_machine
    Rails.configuration.test_server.host
  end

  def test_server_post_url
    "http://#{test_server_machine}/tests"
  end
end

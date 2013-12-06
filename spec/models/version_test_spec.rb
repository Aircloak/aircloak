require 'spec_helper'
require './lib/protobuf_sender'
require './lib/build_manager'

describe VersionTest do
  before(:each) do
    BuildManager.stub(:send_build_request)
    ProtobufSender.stub(:post)
    ProtobufSender.stub(:send_delete)
    Cluster.delete_all
    ClusterCloak.delete_all
    Build.delete_all
    Cloak.delete_all
    DeployableEntityVersion.delete_all
    DeployableEntity.delete_all
    VersionTest.delete_all
  end

  def create_cloaks
    4.times {|n| Cloak.create(name: "test #{n}", tpm: false, ip: "#{n}.#{n}.#{n}.#{n}")}
  end

  let (:entity) { PreRecorded.setup_deployable_entity }
  let (:entity_version) { PreRecorded.setup_deployable_entity_version entity }
  let (:version_test) { entity_version.version_test }

  it "should create a new VersionTest and assign it to the version" do
    version_test.deployable_entity_version.id.should be entity_version.id
  end

  it "should create a new build including the entity under test" do
    version_test.build.deployable_entity_versions.should include(entity_version)
  end

  it "should create a cluster for the test build given the build has completed" do
    create_cloaks
    version_test.mark_build_as_complete.cluster.should_not eq nil
  end

  it "should have a cluster for the given build" do
    create_cloaks
    version_test.mark_build_as_complete.cluster.build.should be version_test.build
  end

  it "should mark itself as failed if it cannot create a cluster" do
    Cloak.all_available.count.should eq 0
    version_test.mark_build_as_complete
    version_test.test_complete.should eq true
    version_test.test_success.should eq false
  end

  it "should mark itself as failed if a build fails" do
    version_test.mark_build_as_failed
    version_test.test_complete.should eq true
    version_test.test_success.should eq false
  end

  it "should should notify the test server when the cluster setup has completed" do
    create_cloaks
    version_test.mark_build_as_complete
    version_test.cluster.stub(:id).and_return(42)
    version_test.mark_cluster_as_ready
  end

  it "should package itself as a test request" do
    create_cloaks
    version_test.mark_build_as_complete
    version_test.save.should eq true
    version_test.as_test_request.id.should eq version_test.id
    version_test.as_test_request.cluster_id.should eq version_test.cluster.id
    version_test.as_test_request.cluster_nodes.each do |node|
      Cloak.find_by_name(node).should_not eq nil
    end
  end

  def results_pb args={}
    TestResponsePB.new(
      id: args[:id] || 1,
      success: args[:success] || false,
      transcript: args[:transcript] || "none"
    )
  end

  it "should mark a test as completed and failed if test server reports failure" do
    version_test.process_result results_pb(success: false)
    version_test.test_complete.should eq true
    version_test.test_success.should eq false
  end

  it "should mark a test as completed and succeeded if test server reports failure" do
    version_test.process_result results_pb(success: true)
    version_test.test_complete.should eq true
    version_test.test_success.should eq true
  end

  it "should mark recall the test transcript" do
    transcript = "this test was so much fun"
    version_test.process_result results_pb(transcript: transcript)
    version_test.test_output.should eq transcript
  end

  it "should delete it's build if the test fails" do
    create_cloaks
    version_test.build.should_not eq nil
    version_test.mark_build_as_failed
    version_test.reload.build.should eq nil
  end

  it "should delete it's build when the test has finished" do
    create_cloaks
    version_test.build.should_not eq nil
    version_test.process_result results_pb
    version_test.reload.build.should eq nil
  end

  it "should delete the build and start the cluster destruction process when the test has finished" do
    create_cloaks
    version_test.mark_build_as_complete
    build = version_test.build
    cluster = version_test.cluster
    cluster.should_receive(:assign_cloaks).with([]) # Starts the deletion of the cluster
    build.should_not eq nil
    cluster.should_not eq nil
    version_test.destroy
    version_test.destroyed?.should eq true
    expect{build.reload}.to raise_error ActiveRecord::RecordNotFound
  end

  it "should produce string results depending on state" do
    version_test.status.should eq "In progress"
    version_test.test_complete = true
    version_test.test_success = true
    version_test.status.should eq "Passed"
    version_test.test_success = false
    version_test.status.should eq "Failed"
  end
end

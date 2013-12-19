require './lib/log_server_configurer'

describe LogServerConfigurer do
  before(:all) do
    begin Cluster
    rescue
      class Cluster; end
    end
    begin Rails
    rescue
      class Rails;
        def self.method_missing m; self end
        def self.host; "localhost" end
      end
    end
  end

  let (:cloak1) { double name: "cloak1-hostname" }
  let (:cloak2) { double name: "cloak2-hostname" }
  let (:cloak3) { double name: "cloak3-hostname" }
  let (:cluster) { double log_name: "LogName", cloaks: [cloak1, cloak2] }

  it "should package each cluster" do
    msg = LogServerConfigurer.proto_for_cluster cluster
    msg.cluster_name.should eq cluster.log_name
    msg.cloak_names.size.should eq 2
    msg.cloak_names.zip(cluster.cloaks.map(&:name)) do |a,b|
      a.should eq b
    end
  end

  it "should make a valid log config message" do
    Cluster.should_receive(:all).and_return([cluster])
    Cluster.should_receive(:available_cloaks).and_return([cloak3])
    msg = LogServerConfigurer.proto
    msg.clusters.size.should eq 1
    msg.clusters.first.cluster_name.should eq cluster.log_name
    msg.unassigned_cloaks.should eq [cloak3.name]
  end

  it "should know the url of the log config server" do
    LogServerConfigurer.server_url.should eq "http://localhost/config"
  end

  it "should send a message to the log server" do
    # Why twice? One for implementation and one for expectation.
    Cluster.should_receive(:all).twice.and_return([cluster])
    Cluster.should_receive(:available_cloaks).twice.and_return([cloak3])
    ProtobufSender.should_receive(:post_to_url).with(LogServerConfigurer.server_url, LogServerConfigurer.proto)
    LogServerConfigurer.update_config
  end
end

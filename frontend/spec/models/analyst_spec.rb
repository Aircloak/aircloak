require 'spec_helper'

describe Analyst do
  before(:each) do
    Analyst.destroy_all
    Cloak.delete_all
    Build.delete_all
    Cluster.delete_all
  end

  let(:cloak) {Cloak.create(name: "test cloak", ip: "127.0.1.2")}
  let(:build) {Build.create(name: "test", manual: true)}
  let(:cluster) {Cluster.create(name: "test cluster: #{analyst.id}", cloaks: [cloak], build: build)}
  let(:analyst) {Analyst.create name: "test analyst"}

  it "should have a name" do
    Analyst.create.errors.should include(:name)
  end

  it "should have a key" do
    Analyst.create(name: "test").key.should_not eq nil
  end

  it "should know if it has clusters or not" do
    analyst.has_clusters?.should eq false
    analyst.should_receive(:clusters).and_return([double(:cluster)])
    analyst.has_clusters?.should eq true
  end

  it "creates and revokes key materials" do
    user = User.new(login: "test", email: "test@aircloak.com", analyst: analyst)
    KeyMaterial.should_receive(:api_ca).and_return(TokenGenerator.generate_root_token("air_web_api", -1))
    ["data_upload_all", "admin", "task_runner", "web_api"].each do |key_type|
      km = KeyMaterial.create_from_user user, "foobar", "desc", key_type
      km.user.should eq user
      km.description.should eq "desc"
      km.key_type.should eq key_type
      km.revoked.should eq false
      token = nil
      if key_type == "web_api"
        km.user_token.should_not eq nil
        token = km.user_token
      else
        km.user_token.should eq nil
      end

      analyst.revoke_key(km)
      km.revoked.should eq true
      km.user_token.should eq nil
      if token
        UserToken.find_by_id(token.id).should eq nil
        UserToken.find_by_token(token.token).should eq nil
      end
    end
  end

  it "should know the difference between one-off and persistent tasks" do
    task = Task.create(
      analyst: analyst,
      name: "test-task",
      code: "lua code",
      prefetch: "prefetch",
      cluster: cluster
    )
    analyst.persistent_tasks.size.should eq 1
    task.one_off = true
    task.save.should eq true
    analyst.reload.persistent_tasks.size.should eq 0
  end

  context "destruction" do
    before(:each) do
      Cloak.delete_all
      Cluster.delete_all
      Task.delete_all
      Build.delete_all
      UserTable.delete_all
      Result.delete_all
      LookupTable.delete_all
      User.delete_all
      Permission.delete_all
    end

    it "should destroy dependents" do
      expect {Cloak.create(name: "test cloak", ip: "127.0.1.9")}.to change { Cloak.count }.from(0).to(1)
      expect {Build.create(name: "test", manual: true)}.to change {Build.count}.from(0).to(1)
      expect {analyst.clusters << Cluster.create(name: "test cluster", cloaks: [Cloak.first], build: Build.first)}.to change {
          Cluster.count }.from(0).to(1)
      expect {analyst.user_tables << UserTable.create(table_name: "test_table", cluster: Cluster.first)}.to change {
          UserTable.count }.from(0).to(1)
      expect {
        prefetch_data = '{"table":"test1","where":{"\$\$priority": {"$lt": 3}}}'
        PrefetchFilter.should_receive(:data_to_prefetch).and_return(prefetch_data)
        Task.create(
          name: "test-task",
          code: "lua code",
          data: prefetch_data,
          cluster: analyst.clusters.first,
          stored_task: false,
          analyst: analyst
        )}.to change { Task.count }.from(0).to(1)
      expect {
          lt = LookupTable.new(
            table_name: "test",
            cluster: analyst.clusters.first,
            analyst: analyst,
            deleted: false,
          )
          lt.upload_data = StringIO.new("[[\"hello\", \"world\"]]")
          lt.save.should eq true
        }.to change {LookupTable.count}.from(0).to(1)
      expect {Result.create(task: analyst.tasks.first, analyst: analyst)}.to change {Result.count}.from(0).to(1)

        # All the crap above, just for this tiny test...
        analyst.destroy.should eq false
        # Can destroy once cluster is gone
        analyst.clusters.delete_all
        analysts_count = Analyst.count
        Analyst.where(id: analyst.id).destroy_all
        Task.count.should eq 0
        UserTable.count.should eq 0
        LookupTable.count.should eq 0
        Result.count.should eq 0
        Analyst.count.should eq analysts_count -1
    end

    def add_user username
      user = User.new(
        password: "abcd",
        password_confirmation: "abcd",
        login: username,
        email: "#{username.gsub(" ", "")}@aircloak.com"
      )
      user.save.should eq true
      analyst.users << user
      user
    end

    it "should remove users" do
      add_user "test"
      expect {analyst.destroy}.to change {User.count}.from(1).to(0)
    end

    it "should not remove admin users who impersonate the analyst" do
      user = add_user "test"
      user.permissions << Permission.create(name: "admin")
      user.reload.admin?.should eq true
      user_count_before = User.count
      analyst.destroy
      User.count.should eq user_count_before
    end
  end
end

require 'spec_helper'

describe AuditLog do
  before(:each) do
    AuditLog.delete_all
    Cloak.delete_all
  end

  context "validations" do
    let(:log) { a = AuditLog.create; a }

    it "must have a raw log message" do
      log.errors.should include(:log_message)
    end

    it "must have a raw log id" do
      log.errors.should include(:log_id)
    end

    it "must have a cloak" do
      log.errors.should include(:cloak_id)
    end
  end

  it "should be able to delete all orphaned log messages" do
    cloak = Cloak.create name: "name", ip: "127.0.0.1"
    [
      [";06FFAC01D2125165F300F760FEB4C7AB460D88A6C0152C6E4601B639494941A0;146;hello;2014-12-19T15:40:05Z;world", cloak.id],
      [";06FFAC01D2125165F300F760FEB4C7AB460D88A6C0152C6E4601B639494941A0;146;hello;2014-12-19T15:40:05Z;worldsssss", cloak.id + 1],
      [";06FFAC01D2125165F300F760FEB4C7AB460D88A6C0152C6E4601B639494941A0;146;hello;2014-12-19T15:40:05Z;worldsssss", cloak.id + 1]
    ].each do |msg, cloak_id|
      AuditLog.create_from_log_message(msg, cloak_id).save
    end
    AuditLog.count.should eq 3
    AuditLog.remove_orphaned_logs
    AuditLog.count.should eq 1
  end
end

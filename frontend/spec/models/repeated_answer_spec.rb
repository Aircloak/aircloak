require 'spec_helper'

describe Analyst do
  before(:each) do
    Analyst.destroy_all
    RepeatedAnswer.delete_all
  end

  let(:analyst) { Analyst.create name: "test analyst" }

  it "should be able to remove orphaned repeated answers" do
    ids = []
    analyst.id.upto(analyst.id + 3) do |id|
      ids << "(#{id})"
    end
    sql = "INSERT INTO repeated_answers (analyst_id) VALUES #{ids.join(", ")}"
    ActiveRecord::Base.connection.execute(sql)
    RepeatedAnswer.count.should eq 4
    RepeatedAnswer.remove_orphaned_reports
    RepeatedAnswer.count.should eq 1
  end
end

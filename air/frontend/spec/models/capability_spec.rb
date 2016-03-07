require 'spec_helper'

describe Capability do
  before(:each) do
    Capability.delete_all
  end

  it "should have an identifier" do
    capability = Capability.new
    capability.save.should eq false
  end

  it "should have a unique identifier" do
    c1 = Capability.new identifier: "test"
    c1.save.should eq true

    c2 = Capability.new identifier: "test"
    c2.save.should eq false
  end
end

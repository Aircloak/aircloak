require 'spec_helper'

describe Cloak do
  context "setting and getting health" do
    let(:cloak) { Cloak.new }

    it "should know what health types are supported" do
      Cloak.health_types.should eq [:unknown, :good, :changing, :sw_failing, :hw_failing]
    end

    it "should default to an unknown health" do
      cloak.health.should eq :unknown
    end

    it "should have the ability to get and set health" do
      Cloak.health_types.each do |health|
        cloak.set_health health
        cloak.health.should eq health
      end
    end
  end
end

require 'spec_helper'

describe Analyst do
  before(:each) do
    Analyst.destroy_all
  end

  it "should have a name" do
    Analyst.create.errors.should include(:name)
  end

  it "should have a key" do
    Analyst.create(name: "test").key.should_not eq nil
  end
end

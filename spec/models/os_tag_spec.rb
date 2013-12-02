require 'spec_helper'

describe OsTag do
  it "should have a name" do
    OsTag.create.errors.should include(:name)
  end

  it "should have a description" do
    OsTag.create.errors.should include(:description)
  end
end

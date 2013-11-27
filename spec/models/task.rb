require './spec/spec_helper'

describe Task do
  before(:each) do
    Query.destroy_all
    Task.destroy_all
  end

  it "should have a name" do
    task1 = Task.new(main_package: "foo", packaged_data: "binary")
    task1.save.should eq false
    task1.errors[:name].should_not eq nil

    task2 = Task.new(name: "task2", main_package: "foo", packaged_data: "binary")
    task2.save.should eq true
  end

  it "should have an unique name" do
    task1 = Task.new(name: "task", main_package: "foo", packaged_data: "binary")
    task1.save.should eq true

    task2 = Task.new(name: "task", main_package: "foo", packaged_data: "binary")
    task2.save.should eq false
    task2.errors[:name].should_not eq nil
  end

  it "update tasks should have a payload identifier" do
    task1 = Task.new(name: "task1", main_package: "foo", packaged_data: "binary", update_task: true)
    task1.save.should eq false
    task1.errors[:payload_identifier].should_not eq nil

    task2 = Task.new(name: "task2", main_package: "foo", packaged_data: "binary", update_task: true,
        payload_identifier: "blarg")
    task2.save.should eq true
  end

  it "should have binary code data" do
    task1 = Task.new(name: "task1", main_package: "foo")
    task1.save.should eq false
    task1.errors[:packaged_data].should_not eq nil

    task2 = Task.new(name: "task2", main_package: "foo", packaged_data: "binary")
    task2.save.should eq true
  end

  it "should have a main package name" do
    task1 = Task.new(name: "task1", packaged_data: "binary")
    task1.save.should eq false
    task1.errors[:main_package].should_not eq nil

    task2 = Task.new(name: "task2", main_package: "foo", packaged_data: "binary")
    task2.save.should eq true
  end

  it "should not remove if a query is associated with it" do
    task1 = Task.new(name: "task1", main_package: "foo", packaged_data: "binary")
    task1.save.should eq true
    task2 = Task.new(name: "task2", main_package: "foo", packaged_data: "binary")
    task2.save.should eq true

    task1.can_destroy?.should eq true
    task1.destroy.destroyed?.should eq true

    query = Query.new(name: "query", task: task2)
    query.save.should eq true
    task2.can_destroy?.should eq false
    task2.destroy.should eq false
  end
end

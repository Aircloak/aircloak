require 'spec_helper'
require 'prefetch_helper'
require './lib/prefetch_filter.rb'

describe PrefetchFilter do
  include PrefetchHelper
  it "converts both ways" do
    prefetch_conversions.each do |data, prefetch|
      data_to_prefetch(data).should eq prefetch
      prefetch_to_data(prefetch).should eq data
    end
  end

  it "returns empty data string on invalid prefetch" do
    PrefetchFilter.prefetch_to_data("[{\"table\":\"non-existing\"}]").should eq ""
    PrefetchFilter.prefetch_to_data("invalid json").should eq ""
  end

  it "fails on invalid table" do
    expect{data_to_prefetch("[{\"tableId\":-1}]", double(analyst: analyst_double([])))}.
        to raise_error(InvalidPrefetchFilter, "invalid table")
  end

  it "fails on empty filter" do
    expect{data_to_prefetch("[]", double())}.to raise_error(InvalidPrefetchFilter, "can't be blank")
  end

  it "fails on invalid operator" do
    expect{data_to_prefetch(table_data([[["age", "???", "1"]]]))}.
        to raise_error(InvalidPrefetchFilter, "invalid operator ???")
  end

  it "fails on invalid column" do
    expect{data_to_prefetch(table_data([[["age1", "=", "1"]]]))}.
        to raise_error(InvalidPrefetchFilter, "invalid column age1")
  end

  it "fails on invalid value" do
    expect{data_to_prefetch(table_data([[["age", "???", "non-integer"]]]))}.
        to raise_error(InvalidPrefetchFilter, "invalid value non-integer for column age")
  end

  private
    def data_to_prefetch(data, task = nil)
      task ||= double(analyst: analyst_double([age_table_double]))
      PrefetchFilter.data_to_prefetch(task, data)
    end

    def prefetch_to_data(prefetch)
      AnalystTable.should_receive(:where).with(table_name: "age").and_return([age_table_double])
      PrefetchFilter.prefetch_to_data(prefetch)
    end
end

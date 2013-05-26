require 'java'
require 'digest/sha1'

class QueryFile < ActiveRecord::Base
  belongs_to :query
  has_many :index_query_files, :dependent => :destroy
  has_many :indices, :through => :index_query_files

  accepts_nested_attributes_for :index_query_files, :allow_destroy => true
  accepts_nested_attributes_for :indices

  def self.perform_json_ops qf
    if qf["scheduleRemove"] then
      remove_query_remnants qf
      return nil
    end

    if qf["fresh"] then
      temp_file = TempQueryFile.find(qf["temp_file"])
      return QueryFile.new(
        name: qf["name"],
        query_interface: qf["query_interface"],
        index_ops: qf["index_ops"],
        package: qf["package"],
        index_ops: qf["index_ops"],
        data: temp_file.data)
    else
      return QueryFile.find(qf["id"])
    end
  end

  def add_indices_from_json qf
    qf["indices"].each do |i|
      self.indices << Index.from_json(i)
    end
  end

private
  def self.remove_query_remnants qf
    if qf["fresh"] then
      TempQueryFile.destroy(qf["temp_file"])
    else
      QueryFile.destroy(qf["id"])
    end
  end
end

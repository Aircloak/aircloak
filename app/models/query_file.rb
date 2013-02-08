require 'java'
require 'digest/sha1'

class QueryFile < ActiveRecord::Base
  belongs_to :query
  has_many :index_query_files, :dependent => :destroy
  has_many :indices, :through => :index_query_files

  accepts_nested_attributes_for :index_query_files, :allow_destroy => true
  accepts_nested_attributes_for :indices
end

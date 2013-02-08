class Index < ActiveRecord::Base
  has_many :index_query_files, :dependent => :destroy
  has_many :query_files, :through => :index_query_files
end

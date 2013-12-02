class OsTag < ActiveRecord::Base
  has_many :clusters

  validates_presence_of :name, :description
end

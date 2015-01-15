class Capability < ActiveRecord::Base
  validates_presence_of :identifier
  validates_uniqueness_of :identifier

  has_many :capability_clusters, dependent: :destroy
  has_many :clusters, through: :capability_clusters
end

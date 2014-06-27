require './lib/token_generator'

class Analyst < ActiveRecord::Base
  has_and_belongs_to_many :clusters
  has_many :tasks
  has_many :analyst_tables
  has_many :results

  has_many :analysts_clusters
  has_and_belongs_to_many :clusters

  validates_presence_of :name

  after_create :create_token, :unless => :key
  before_destroy :can_destroy

private
  def create_token
    self.key, self.certificate = TokenGenerator.generate_root_token("analyst", self.id)
    save
    true
  end

  def can_destroy
    if clusters.count > 0
      self.errors.add(:analyst, "Cannot destroy an analyst that is assigned to a cluster.")
      false
    end
  end
end

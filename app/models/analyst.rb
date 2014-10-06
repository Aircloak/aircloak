require './lib/token_generator'

class Analyst < ActiveRecord::Base
  has_and_belongs_to_many :clusters
  has_many :tasks
  has_many :user_tables
  has_many :lookup_tables
  has_many :results
  has_many :users
  has_many :key_materials

  has_many :analysts_clusters
  has_and_belongs_to_many :clusters

  has_many :repeated_answers

  validates_presence_of :name

  after_create :create_token, :unless => :key
  before_destroy :can_destroy

  def self.analyst_options
    [["None", "none"]] + Analyst.all.map {|a| [a.name, a.id]}
  end

  def undeleted_user_tables
    user_tables.where(deleted: false)
  end

  def tasks_with_exceptions
    tasks.select {|t| t.has_exceptions? }
  end

  def revoke_key key
    self.revocation_list = TokenGenerator.revoke_certificate self.key, self.revocation_list, key.certificate
    save
    key.revoked = true
    key.save
    # mark all assigned clusters as needing to be updated with the new revocation list
    self.clusters.each do |cluster|
      cluster.mark_as_changed
      cluster.save
    end
  end

private
  def create_token
    create_analyst_token
    create_inquirer_token
    save
    true
  end

  def create_analyst_token
    self.key, self.certificate = TokenGenerator.generate_root_token "analyst", self.id
    self.revocation_list = TokenGenerator.generate_empty_revocation_list self.key, self.certificate
  end

  def create_inquirer_token
    # We use id = 0 for the generic inquirer token that is stored with the
    # analyst object.
    self.inquirer_key, self.inquirer_cert =
        TokenGenerator.generate_leaf_token self.key, self.certificate, "inquirer", 0
  end

  def can_destroy
    if clusters.count > 0
      self.errors.add(:analyst, "Cannot destroy an analyst that is assigned to a cluster.")
      false
    end
  end
end

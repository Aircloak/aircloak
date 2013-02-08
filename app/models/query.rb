class Query < ActiveRecord::Base
  has_many :query_files, :dependent => :destroy
  belongs_to :index

  accepts_nested_attributes_for :query_files, allow_destroy: true

  validates :name, :index_id, :presence => true
  validate :existing_identifier

  before_validation :should_have_identifier?

  def should_have_identifier?
    unless update_query then
      self.identifier = ""
    end
  end

  def existing_identifier
    errors.add(:identifier, "is required for update queries") if update_query && identifier.blank?
  end

  # The name of whom this query runs on behalf of
  def on_behalf_of
    system_query ? "Aircloak" : "Analyst"
  end
end

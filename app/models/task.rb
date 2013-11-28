class Task < ActiveRecord::Base
  has_many :queries

  validates :packaged_data, :main_package, :presence => true
  validates_uniqueness_of :main_package
  validate :existing_payload_identifier

  before_validation :should_have_payload_identifier?
  before_destroy :validate_destroyability

  def can_destroy?
    queries.count == 0
  end

private

  def should_have_payload_identifier?
    self.payload_identifier = "" unless update_task?
  end

  def existing_payload_identifier
    if update_task? && payload_identifier.blank?
      errors.add(:payload_identifier, "is required for update tasks")
    end
  end

  def validate_destroyability
    unless can_destroy?
      self.errors.add(:queries, "cannot destroy a task with assigned queries")
      return false
    end
  end
end

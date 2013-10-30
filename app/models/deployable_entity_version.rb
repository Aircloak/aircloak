class DeployableEntityVersion < ActiveRecord::Base
  belongs_to :deployable_entity
  before_save :set_message_and_author

  validates_presence_of :deployable_entity_id
  validates_uniqueness_of :commit_id

private
  def set_message_and_author
    Gh.add_message_and_author self
  end
end

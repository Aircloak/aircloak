class BuildVersion < ActiveRecord::Base
  belongs_to :deployable_entity_version
  belongs_to :build
end

require './lib/finger_print_creator.rb'
require './lib/build_manager.rb'

class Build < ActiveRecord::Base
  # This before_destroy callback needs to be called
  # before the object is destroyed. The object
  # might get destroyed through a :dependent => :destroy
  # from the DeployableEntity. It should not be allowed to
  # finish the destroy if there is a build
  # relying on this version.
  # Since the :dependent => :destroy mechanism itself
  # relies on callbacks we need to ensure our callback
  # is defined before the other callbacks, in
  # order for it to be executed first.
  before_destroy :validate_destroyability

  validates_presence_of :name
  validates_uniqueness_of :name
  validates_uniqueness_of :fingerprint, message: "is not unique. " +
      "There exists another build with the same versions of the deployable entities"
  
  before_validation(on: :create) do
    self.fingerprint = FingerPrintCreator.fingerprint self
  end

  after_create :send_request_for_building

  has_many :build_versions
  has_many :deployable_entity_versions, through: :build_versions
  has_many :clusters

  # A build that is used for an automated test of a commit will have a
  # version_test instance. For all other builds this will be nil
  has_one :version_test

  # A build has associated with it a set of deployable entity versions.
  # In the form for creating builds we want to be able to remember which
  # deployable entity versions that are part of a build.
  def version_for_entity entity
    vid = nil
    deployable_entity_versions.each do |v|
      vid = v.id if v.deployable_entity_id == entity.id
    end
    return vid || 0
  end

  def can_destroy?
    clusters.blank? && version_test.blank?
  end

  def mark_complete args={}
    self.build_completed = true
    self.build_success = args[:success] || false
    if version_test
      version_test.mark_build_as_complete if args[:success]
      version_test.mark_build_as_failed unless args[:success]
    end
    save
  end

  def status_message
    building = false
    deployable_entity_versions.each do |dev|
      building = true unless dev.build_completed
      return "Failed" if dev.build_success == false
    end
    if building
      "Building"
    else
      "Built"
    end
  end

private
  def validate_destroyability
    if not can_destroy?
      self.errors.add(:clusters, "Cannot destroy a build with clusters")
      false
    end
  end

  def send_request_for_building
    BuildManager.send_build_request self if Rails.configuration.installation.global
  end
end

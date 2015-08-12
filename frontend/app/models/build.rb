require './lib/finger_print_creator.rb'
require './lib/build_manager.rb'
require './lib/aircloak_config.rb'

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
  after_destroy :remove_from_buildserver, unless: :manual

  validates_presence_of :name
  validates_uniqueness_of :name
  validates_format_of :name, if: :manual, :with => /\A[a-zA-Z0-9\-_\/]+\Z/, message: " is not valid for manual builds."
  validates_uniqueness_of :fingerprint, unless: :manual, message: "is not unique. " +
      "There exists another build with the same versions of the deployable entities"

  before_validation(on: :create) do
    self.fingerprint = FingerPrintCreator.fingerprint self
  end

  after_create :send_request_for_building, unless: :manual

  has_many :build_versions
  has_many :deployable_entity_versions, through: :build_versions
  has_many :clusters

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
    clusters.blank?
  end

  def mark_complete args={}
    self.build_completed = true
    self.build_success = args[:success] || false
    ActiveRecord::Base.connection_pool.with_connection do |connection|
      connection.execute "NOTIFY build, '#{self.id}'"
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

  def reset
    unless self.manual then
      deployable_entity_versions.each do |dev|
        dev.reset_build_status
      end
      send_request_for_building
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
    if Conf.get("/settings/rails/global") == "true"
      BuildManager.send_build_request self
    else
      # When we develop, we fake the creation of the build,
      # since we don't have the build server running locally
      mark_complete(success: true)
      deployable_entity_versions.each do |dev|
        dev.build_completed = true
        dev.build_success = true
        dev.save
      end
    end
  end

  def remove_from_buildserver
    unless Rails.env.development?
      url = "http://#{Conf.get("/service/build_server/host")}/build/#{self.id}"
      ProtobufSender.send_delete url if Conf.get("/settings/rails/global") == "true"
    end
  end
end

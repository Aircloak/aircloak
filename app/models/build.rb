require './lib/finger_print_creator.rb'
require './lib/build_manager.rb'

class Build < ActiveRecord::Base
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

private
  def send_request_for_building
    build_request = BuildManager.create_build_request self
    
    url = URI.parse("http://cloakbuild.mpi-sws.org/build")
    https = Net::HTTP.new(url.host, url.port)
    request = Net::HTTP::Post.new(url.path)
    request.content_type = "application/x-protobuf"
    request.body = build_request.encode.buf
    result = https.request(request)
    unless result.code == "201" then
      self.build_completed = true
      self.build_success = false
      save
    end
  end
end

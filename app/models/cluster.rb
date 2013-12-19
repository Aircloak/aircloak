require './lib/cluster_packer.rb'
require './lib/log_server_configurer'

class Cluster < ActiveRecord::Base
  has_many :cluster_cloaks
  has_many :cloaks, through: :cluster_cloaks
  belongs_to :build
  belongs_to :os_tag

  has_many :queries, dependent: :destroy

  # A cluster that is used for an automated test of a commit will have a
  # version_test instance. For all other clusters this will be nil
  has_one :version_test

  validates :name, presence: true, uniqueness: true
  validates_presence_of :build, :os_tag
  validate :must_match_tpm_configuration
  validate :must_have_at_least_one_cloak

  after_save :after_save_inform_mannyair
  after_commit :update_log_server
  before_destroy :verify_can_destroy

  def tpm
    @has_tpm ||= build.tpm
  end

  def num_broken
    cloaks.inject(0) {|res, cloak| cloak.good ? res : res + 1 }
  end

  def health
    num_broken > 0 ? :poor : :healthy
  end

  def available_cloaks
    global_available = Cloak.all_available
    available_cloaks = (cloaks + global_available).sort { |a, b| a.name <=> b.name }
  end

  def selected_cloaks
    cloaks.map(&:id)
  end

  def assign_cloaks new_cloaks
    old_cloaks = self.cloaks.to_a
    (new_cloaks - old_cloaks).each {|cloak| cloaks << cloak }
    (new_cloaks & old_cloaks).each {|cloak| keep_cloak cloak }
    (old_cloaks - new_cloaks).each {|cloak| cloak.cluster_cloak.set_state :to_be_removed }
  end

  def keep_cloak cloak
    cloak.cluster_cloak.set_state :to_be_added unless cloak.cluster_cloak.state == :belongs_to
  end

  # Creates a cluster for testing a particular build.
  # It will be assigned three random cloak computers
  # that do not have TPMs (these are likely to be VMs,
  # but this might not be true in the future).
  def self.test_cluster_for_build build
    cloaks = Cloak.cloaks_for_build_testing
    Cluster.create(build: build, name: "Test cluster - #{build.name}", cloaks: cloaks, os_tag: OsTag.last)
  end

  def timestamp
    updated_at.to_i
  end

  def cloak_ready
    return unless version_test
    cluster_cloaks.each do |cc|
      return unless cc.state == :belongs_to
    end
    version_test.mark_cluster_as_ready
  end

  def self.ready_clusters
    clusters = ClusterCloak.where(raw_state: ClusterCloak.state_to_raw_state(:belongs_to)).map(&:cluster)
    sorted_cluster = clusters.sort { |a, b| a.name <=> b.name }
    sorted_cluster.uniq { |cluster| cluster.name }
  end

  def ready?
    cluster_cloaks.each do |cc|
      return true if cc.state == :belongs_to
    end
    return false
  end

  def can_destroy?
    version_test.blank? and cloaks.count == 0
  end

  # A log name is a version of the cluster name that
  # is sane for using in folder and file names on the log server
  def log_name
    name_base = "#{name}-#{build.name}-#{os_tag.name}"
    name_base.gsub(" ", "_").gsub(/[^\w^\d^\-]*/, "")
  end

private
  def must_match_tpm_configuration
    unless !build || cloaks.inject(true) {|is_ok, cloak| is_ok && cloak.tpm == self.tpm }
      self.errors.add :cloaks, "must match tpm configuration"
    end
  end

  def must_have_at_least_one_cloak
    self.errors.add :cloaks, "must have at least one cloak" unless cloaks.size > 0
  end

  def mannyair_post_url
    "http://#{Rails.configuration.manny_air.host}/clusters"
  end

  def after_save_inform_mannyair
    cp = ClusterPacker.package_cluster self
    ProtobufSender.post_to_url mannyair_post_url, cp
  end

  def verify_can_destroy
    unless can_destroy?
      self.errors.add(:version_test, "Cannot destroy a build used by a version test")
      false
    end
  end

  def update_log_server
    LogServerConfigurer.update_config
  end
end

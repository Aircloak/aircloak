require './lib/cluster_packer.rb'
require './lib/log_server_configurer'

class Cluster < ActiveRecord::Base
  has_many :cluster_cloaks
  has_many :cloaks, through: :cluster_cloaks
  has_many :analyst_tables
  has_many :tasks
  # A cluster that is used for an automated test of a commit will have a
  # version_test instance. For all other clusters this will be nil
  has_one :version_test
  has_many :analysts_clusters
  has_and_belongs_to_many :analysts
  belongs_to :build

  validates :name, presence: true, uniqueness: true
  validates_presence_of :build
  validate :must_have_at_least_one_cloak
  validate :must_match_tpm_configuration

  after_commit :update_log_server
  after_commit :synchronize_in_local_mode
  before_destroy :verify_can_destroy
  after_destroy :remove_tasks

  def tpm
    return "Unkown" unless cloaks.size > 0
    @has_tpm ||= cloaks.first.tpm
  end

  def random_cloak_ip
    cloaks.sample.ip
  end

  def num_broken
    cloaks.inject(0) {|res, cloak| cloak.good ? res : res + 1 }
  end

  def health
    num_broken > 0 ? :poor : :healthy
  end

  def available_cloaks
    global_available = Cloak.all_available
    (cloaks + global_available).sort { |a, b| a.name <=> b.name }
  end

  def selected_cloaks
    cloaks.map(&:id)
  end

  def assign_cloaks new_cloaks
    if ! verify_cloaks_match new_cloaks then
      return false
    end
    old_cloaks = self.cloaks.to_a
    (new_cloaks - old_cloaks).each {|cloak| cloaks << cloak }
    (new_cloaks & old_cloaks).each {|cloak| keep_cloak cloak }
    (old_cloaks - new_cloaks).each {|cloak| cloak.cluster_cloak.set_state :to_be_removed }
    return true
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
    Cluster.create(build: build, name: "test-#{build.name}", cloaks: cloaks)
  end

  def timestamp
    last_modified.to_i
  end

  def cloak_ready
    return unless version_test
    cluster_cloaks.each do |cc|
      return unless cc.state == :belongs_to
    end
    version_test.mark_cluster_as_ready
  end

  def self.ready_clusters analyst
    params = {
      analyst_id: analyst.id,
      raw_state: ClusterCloak.state_to_raw_state(:belongs_to)
    }
    Cluster.joins(:analysts_clusters, :cluster_cloaks)
        .where("cluster_cloaks.raw_state = :raw_state and " +
            "analysts_clusters.cluster_id = clusters.id and " +
            "analysts_clusters.analyst_id = :analyst_id", params)
        .order(:name)
        .uniq
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
    name_base = "#{name}-#{build.name}"
    name_base.gsub(" ", "_").gsub(/[^\w^\d^\-]*/, "")
  end

  def status= raw_status
    self.status_value = status_mappings[raw_status]
    if raw_status == :active then
      self.status_description = ""
    end
  end

  def status
    status_mappings.invert[status_value]
  end

  def status_for_display
    msg = status.to_s.humanize
    msg = "#{msg}: #{status_description}" if status != :active
    msg
  end

  def status_description=(description)
    usable_description = description
    if description.size > 255
      usable_description = description[0...252] + "..."
    end
    write_attribute(:status_description, usable_description)
  end

  def cloak_names
    self.cloaks.map {|cloak| cloak.name}
  end

  def get_root_certificates
    # we first load the certificate for the cluster's supervisor machine (manny-air)
    certificates = Cluster.get_root_certificates
    self.analysts.each do |analyst|
      # add the certificates of the assigned analysts to the trusted list of CAs
      certificates += "\n" + analyst.certificate
    end unless self.id == nil
    return certificates
  end

  def self.get_root_certificates
    File.read("config/supervisor.crt")
  end

private
  def verify_cloaks_match new_cloaks
    if ! new_cloaks.inject(true) {|is_ok, cloak| is_ok && cloak.tpm == new_cloaks.first.tpm } then
      self.errors.add :cloaks, "must match tpm configuration"
      return false
    end
    return true
  end

  def must_match_tpm_configuration
    verify_cloaks_match self.cloaks
  end

  def must_have_at_least_one_cloak
    self.errors.add :cloaks, "must have at least one cloak" unless cloaks.size > 0
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

  def synchronize_in_local_mode
    unless Rails.configuration.installation.global
      cluster_cloaks.each {|cluster_cloak| cluster_cloak.synchronize }
    end
  end

  def status_mappings
    {
      active: 1,
      in_service: 2,
      inactive: 3
    }
  end

  def remove_tasks
    tasks.each do |task|
      task.efficient_delete
    end
  end
end

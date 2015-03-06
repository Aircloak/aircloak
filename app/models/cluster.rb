require './lib/cluster_packer.rb'
require 'rest-client'

class Cluster < ActiveRecord::Base
  has_many :cluster_cloaks
  has_many :cloaks, through: :cluster_cloaks
  has_many :user_tables, dependent: :destroy
  has_many :lookup_tables, dependent: :destroy
  has_many :tasks
  has_many :analysts_clusters
  has_many :alterations, as: :target, dependent: :destroy
  has_and_belongs_to_many :analysts
  belongs_to :build
  has_many :capability_clusters
  has_many :capabilities, through: :capability_clusters

  has_many :repeated_answers
  has_many :ra_task_code_clusters, dependent: :destroy
  has_many :ra_task_codes, through: :ra_task_code_clusters

  validates :name, presence: true, uniqueness: true
  validates_presence_of :build
  validate :must_have_at_least_one_cloak
  validate :must_match_tpm_configuration

  before_save :synchronize_in_local_mode
  before_destroy :verify_can_destroy
  after_destroy :remove_state

  # Only add the task code if it does not already belongs to the cluster.
  def add_task_code code
    ra_task_codes << code unless RaTaskCodeCluster.where(cluster: self, ra_task_code: code).count > 0
  end

  def tpm
    raise "Cluster has no cloaks. Unsure if TPM cluster or not" unless cloaks.size > 0
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
    (cloaks + global_available).sort { |a, b| a.name <=> b.name }.uniq { |a| a.name }
  end

  def selected_cloaks
    cloaks.map(&:id)
  end

  def assign_cloaks new_cloaks
    if ! verify_cloaks_match new_cloaks then
      return false
    end
    old_cloaks = self.cloaks.to_a
    added_cloaks = new_cloaks - old_cloaks
    removed_cloaks = old_cloaks - new_cloaks
    kept_cloaks = new_cloaks & old_cloaks

    added_cloaks.each {|cloak| cloaks << cloak }
    kept_cloaks.each {|cloak| keep_cloak cloak }
    removed_cloaks.each {|cloak| cloak.cluster_cloak.set_state :to_be_removed }

    # log changes to cluster
    added_cloaks_list = added_cloaks.map {|cloak| cloak.name}.join(",")
    removed_cloaks_list = removed_cloaks.map {|cloak| cloak.name}.join(",")
    log_alteration "Added cloak(s): '#{added_cloaks_list}'." unless added_cloaks_list.empty?
    log_alteration "Removed cloak(s): '#{removed_cloaks_list}'." unless removed_cloaks_list.empty?

    return true
  end

  def assign_analysts new_analysts
    # log cluster changes
    old_analysts = self.analysts.to_a
    added_analysts = new_analysts - old_analysts
    removed_analysts = old_analysts - new_analysts
    added_analysts_list = added_analysts.map {|analyst| analyst.name}.join(",")
    removed_analysts_list = removed_analysts.map {|analyst| analyst.name}.join(",")
    log_alteration "Added analyst(s): '#{added_analysts_list}'." unless added_analysts_list.empty?
    log_alteration "Removed analyst(s): '#{removed_analysts_list}'." unless removed_analysts_list.empty?

    self.analysts = new_analysts
    return true
  end

  def keep_cloak cloak
    cloak.cluster_cloak.set_state :to_be_added unless \
      cloak.cluster_cloak.state == :belongs_to or cloak.cluster_cloak.state == :to_be_upgraded
  end

  def timestamp
    last_modified.to_i
  end

  def capable_of? identifier
    raise UnknownCapability.new if Capability.where(identifier: identifier).count == 0
    capabilities.where(identifier: identifier).count == 1
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
    cloaks.count == 0
  end

  def can_delete?
    cluster_cloaks.any? { |cluster_cloak| cluster_cloak.state != :to_be_removed }
  end

  def status= raw_status
    self.status_value = status_mappings[raw_status]
    if raw_status == :active then
      self.status_description = ""
      self.last_active = Time.now
    end
  end

  def status
    status_mappings.invert[status_value]
  end

  def status_for_display
    msg = status.to_s.humanize
    msg = "#{msg}: #{status_description}" if status_description != nil && status_description != ""
    msg
  end

  def status_description=(description)
    usable_description = description
    if description.size > 255
      usable_description = description[0...252] + "..."
    end
    write_attribute(:status_description, usable_description)
  end

  def cloak_internal_domains
    self.cloaks.map {|cloak| cloak.internal_domain}
  end

  def get_client_credentials
    # sends client credentials update only when cluster not yet active
    return "", "" if self.status == :active
    # we first load the certificate and revocation list for the cluster's supervisor machine (manny-air)
    certificates, revocation_lists = Cluster.get_client_credentials
    self.analysts.each do |analyst|
      # add the certificates of the assigned analysts to the trusted list of CAs
      certificates += "\n" + analyst.certificate
      revocation_lists += "\n" + analyst.revocation_list
    end unless self.id == nil
    return certificates, revocation_lists
  end

  def self.get_client_credentials
    return File.read("config/supervisor.crt"), File.read("config/supervisor.crl")
  end

  def update_params params
    if params[:build_id].to_i != self.build_id then
      self.cluster_cloaks.each do |cluster_cloak|
        if cluster_cloak.state == :belongs_to  then
          cluster_cloak.set_state(:to_be_upgraded)
        end
      end
      build = Build.find(params[:build_id].to_i)
      log_alteration "Build upgraded to '#{build.name}'"
    end
    if params[:name] != self.name then
      log_alteration "Name changed to '#{params[:name]}'."
    end
    update(params)
  end

  def mark_as_changed
    self.last_modified = Time.now
    self.status = :changes_pending
    self.status_description = ""
  end

  # Performs a check against the cloak, requesting it to list
  # it's capabilities. This way the web interface will automatically
  # show the right interfaces that are supported.
  def check_capabilities
    url = if Rails.configuration.installation.global
      "https://#{random_cloak_ip}/capabilities"
    else
      # We are running in local mode
      protocol = Rails.configuration.cloak.protocol
      port = Rails.configuration.cloak.port
      "#{protocol}://#{random_cloak_ip}:#{port}/capabilities"
    end
    RestClient::Request.execute(method: :get, url: url, timeout: 0.3, open_timeout: 0.2) do |response, request, result, &block|
      case response.code
      when 200
        json = JSON.parse response
        if json["success"]
          json["capabilities"].each do |capability_identifier|
            capability = Capability.find_by_identifier capability_identifier
            capabilities << capability unless capabilities.include? capability
          end
        end
      when 404
        # The cluster hasn't implemented the /capabilities
        # interface. It is most likely a version 1.0.1 cloak
        # which does not support any additional functionalities
        logger.info "Cluster #{name} does not support capabilities"
      end
    end
  rescue Errno::ECONNREFUSED => e
    logger.error "Unable to connect to cluster #{name} to establish capabilities"
  rescue RestClient::RequestTimeout => e
    logger.error "Request to establish capabilities of cluster #{name} timed out"
  end

  def log_alteration description
    user = Authorization.current_user
    user = nil unless user.kind_of?(User) # nil for anonymous users
    alteration = Alteration.new description: description, user: user
    self.alterations << alteration
  end

private
  def verify_cloaks_match new_cloaks
    if ! new_cloaks.inject(true) {|is_ok, cloak| is_ok && cloak.tpm == new_cloaks.first.tpm } then
      self.errors.add :cloaks, "must match tpm configuration"
      return false
    end
    return true
  end

  def synchronize_in_local_mode
    unless Rails.configuration.installation.global
      self.status = :active
      cluster_cloaks.each {|cluster_cloak| cluster_cloak.synchronize }
    end
  end

  def must_match_tpm_configuration
    verify_cloaks_match self.cloaks
  end

  def must_have_at_least_one_cloak
    self.errors.add :cloaks, "must have at least one cloak" unless cloaks.size > 0
  end

  def verify_can_destroy
    unless can_destroy?
      self.errors.add(:cloaks, "Can only destroy a cluster without any cloaks")
      false
    end
  end

  def status_mappings
    {
      active: 1,
      in_service: 2,
      inactive: 3,
      changes_pending: 4
    }
  end

  def remove_state
    # We keep tasks and task results so the analyst can inspect them later
    AuditLog.where(cloak_id: cloaks.map(&:id)).delete_all
  end
end

class UnknownCapability < RuntimeError
end

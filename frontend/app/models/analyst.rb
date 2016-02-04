require './lib/token_generator'
require './lib/migrator'

class Analyst < ActiveRecord::Base
  has_many :analysts_clusters
  has_and_belongs_to_many :clusters
  has_many :tasks, dependent: :destroy
  has_many :user_tables, dependent: :destroy
  has_many :lookup_tables, dependent: :destroy
  has_many :results, dependent: :destroy
  has_many :users
  has_many :key_materials, dependent: :destroy

  validates_presence_of :name

  after_create :create_token, :unless => :key
  before_destroy :pre_destroy_cleanup

  def self.analyst_options
    [["None", "none"]] + Analyst.all.map {|a| [a.name, a.id]}
  end

  def persistent_tasks
    tasks.where(one_off: false)
  end

  def shared_tasks
    persistent_tasks.where(deleted: false, shared: true)
  end

  def private_tasks user
    persistent_tasks.where(deleted: false, shared: false, user_id: user.id)
  end

  def all_private_tasks
    persistent_tasks.where(deleted: false, shared: false)
  end

  def undeleted_user_tables
    user_tables.where(deleted: false)
  end

  def tasks_with_exceptions
    Task
        .where([
              "
                tasks.id IN (
                  SELECT task_id FROM results
                  INNER JOIN exception_results ON result_id=results.id
                  WHERE tasks.analyst_id=?
                ) AND deleted = false
              ",
              [self.id]
            ])
        .select {|t| t.has_exceptions?}
  end

  def revoke_key key
    if key.user_token.nil?
      self.revocation_list = TokenGenerator.revoke_certificate self.key, self.revocation_list, key.certificate
      save
      key.revoked = true
      key.save
      # mark all assigned clusters as needing to be updated with the new revocation list
      self.clusters.each do |cluster|
        cluster.log_alteration "Revoked key '#{key.description}' created by '#{key.user.login}@#{name}'."
        cluster.mark_as_changed
        cluster.save
      end
    else
      key.user_token.destroy
      key.user_token = nil
      key.revoked = true
      key.save
    end
  end

  def has_clusters?
    clusters.count > 0
  end

  def can_destroy?
    not has_clusters?
  end

  def cleanup_cluster cluster
    # delete all tasks
    persistent_tasks.where(cluster_id: cluster.id, deleted: false).each do |task|
      task.active = false
      task.deleted = true
      task.save_and_synchronize!
    end
    # delete user tables from cluster
    undeleted_user_tables.where(cluster_id: cluster.id).each do |table|
      migration = UserTableMigration.drop_migration table.table_name
      table.pending_delete = true
      if not Migrator.migrate table, migration
        raise "Could not drop user table #{table.table_name} on cluster #{cluster.name}"
      end
    end
    # remove lookup tables from clusters
    lookup_tables.where(cluster_id: cluster.id).each do |table|
      result = table.remove
      if not result["success"]
        raise "Could not drop lookup table #{table.table_name} on cluster #{cluster.name}\n"
      end
      table.update! deleted: true
    end
    return true
  end

private
  def create_token
    create_analyst_token
    create_admin_token
    create_task_runner_token
    save
    true
  end

  def create_analyst_token
    self.key, self.certificate = TokenGenerator.generate_root_token "analyst", self.id
    self.revocation_list = TokenGenerator.generate_empty_revocation_list self.key, self.certificate
  end

  def create_admin_token
    # We use id = 0 for the generic admin token that is stored with the
    # analyst object.
    self.admin_key, self.admin_cert =
        TokenGenerator.generate_leaf_token self.key, self.certificate, "admin", 0
  end

  def create_task_runner_token
    # We use id = 0 for the generic task_runner token that is stored with the
    # analyst object.
    self.task_runner_key, self.task_runner_cert =
        TokenGenerator.generate_leaf_token self.key, self.certificate, "task_runner", 0
  end

  def pre_destroy_cleanup
    unless can_destroy?
      self.errors.add(:analyst, "Cannot destroy an analyst that is assigned to a cluster.")
      return false
    end
    # To speed up the destruction, we clear all task results before destroying the tasks
    tasks.each do |task|
      task.efficiently_delete_results
    end
    # We remove all users who are non-admins. Admin users would be Aircloak
    # users who impersonate an analyst. They should not be removed.
    users.each do |user|
      user.destroy unless user.admin?
    end
  end
end

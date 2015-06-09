class User < ActiveRecord::Base
  has_many :user_permissions
  has_many :permissions, through: :user_permissions
  has_many :activities
  belongs_to :analyst
  has_many :tasks, dependent: :destroy

  validate :only_aircloakers_can_be_admin

  before_destroy :remove_tracked_activity

  acts_as_authentic do |c|
    crypto_provider = Authlogic::CryptoProviders::BCrypt
  end

  def attempt_to_make_a_human_name_from_login
    login.split(".").map(&:capitalize).join(" ")
  end

  def needs_onboarding?
    not (has_tables? and has_tasks? and has_keys?)
  end

  def tables_count
    analyst.user_tables.count
  end

  def has_tables?
    tables_count != 0
  end

  def tasks_count
    analyst.tasks.count
  end

  def has_tasks?
    tasks_count != 0
  end

  def has_keys?
    analyst.key_materials.count != 0
  end

  def roles
    role_symbols.map(&:to_s).map(&:humanize).join(", ")
  end

  def role_symbols
    permission_symbols = permissions.map(&:name).map(&:to_sym)
    permission_symbols << :inquirer if analyst
    permission_symbols
  end

  def admin?
    role_symbols.include? :admin
  end

  def inquirer?
    role_symbols.include? :inquirer
  end

  def has_permissions?
    role_symbols.count != 0
  end

  def has_multiple_clusters?
    analyst.clusters.count > 1
  end

  def on_behalf_of
    entities = []
    entities << "Aircloak" if role_symbols.include? :admin
    entities << analyst.name if analyst
    if entities == []
      "No affiliation"
    else
      entities.join ", "
    end
  end

  def managed_users
    if admin?
      User.all
    else
      analyst.users.select do |user|
        not user.admin?
      end
    end
  end

  def new_user_from_params params, analyst_id
    if admin?
      new_user = User.new params
      new_user.analyst = Analyst.find analyst_id if analyst_id != "none"
      new_user
    else
      # To prevent privilege escalation from non-admin to
      # admin users, and likewise to prevent a non-admin user
      # from guessing permission ids and assigning a new user
      # random permissions, we outright prevent a non-admin user
      # from creating users with permissions.
      params.delete "permission_ids"
      analyst.users.new params
    end
  end

  def scoped_find id
    if admin?
      User.find id
    else
      user = analyst.users.find id
      if user.admin? then
        raise ActiveRecord::RecordNotFound.new
      else
        user
      end
    end
  end

  def ready_clusters
    Cluster.ready_clusters(self.analyst)
  end

  def remove_tracked_activity
    Activity.where(user_id: self.id).delete_all
  end

private
  def only_aircloakers_can_be_admin
    unless email =~ /aircloak\.com$/
      admin_permission = Permission.find_by_name "admin"
      errors[:base] << "Non-aircloakers are not allow to be administrators" if permission_ids.include? admin_permission.id
    end
  end
end

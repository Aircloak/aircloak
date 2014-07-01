class User < ActiveRecord::Base
  has_many :user_permissions
  has_many :permissions, through: :user_permissions
  belongs_to :analyst

  acts_as_authentic do |c|
    crypto_provider = Authlogic::CryptoProviders::BCrypt
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
      analyst.users
    end
  end

  def new_user_from_params params, analyst_id
    if admin?
      new_user = User.new params
      new_user.analyst = Analyst.find analyst_id if analyst_id != "none"
      new_user
    else
      analyst.users.new params
    end
  end

  def scoped_find id
    if admin?
      User.find id
    else
      analyst.users.find id
    end
  end

  def ready_clusters
    Cluster.ready_clusters(self.analyst)
  end
end

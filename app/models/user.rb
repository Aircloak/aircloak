class User < ActiveRecord::Base
  has_many :user_permissions
  has_many :permissions, through: :user_permissions
  belongs_to :analyst

  acts_as_authentic do |c|
    crypto_provider = Authlogic::CryptoProviders::BCrypt
  end

  def role_symbols
    permission_symbols = permissions.map(&:name).map(&:to_sym)
    permission_symbols << :inquirer if analyst
    permission_symbols
  end

  def admin?
    role_symbols.include? :admin
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

  def ready_clusters
    Cluster.ready_clusters(self.analyst)
  end
end

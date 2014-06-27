authorization do
  role :guest do
    has_permission_on :welcome, to: :index
    has_permission_on :get_latest, to: :show
    has_permission_on :user_sessions, to: [:new, :create, :destroy]
    has_permission_on :users, to: [:show, :create, :update] do
      if_attribute :user => is {user}
    end
    has_permission_on [
      :machines,
      :cluster_lists,
      :api_tasks
    ], to: :anon_read
    has_permission_on [
      :results,
      :register_version,
      :build_progress,
      :machines,
      :tasks,
      :version_tests,
      :api_tasks,
      :api_clusters
    ], to: :anon_write
  end

  role :inquirer do
    includes :guest
    has_permission_on [
      :tasks,
      :results,
      :tables
    ], to: :manage
  end

  role :admin do
    includes [:guest, :inquirer]
    has_permission_on [
      :tasks,
      :results,
      :users,
      :deployable_entities,
      :deployable_entity_versions,
      :builds,
      :clusters,
      :cloaks,
      :analysts,
      :permissions,
      :version_tests,
      :user_permissions,
      :metrics,
      :tables
    ], to: :manage
    has_permission_on [:tables], to: :retry_migration
    has_permission_on [:authorization_rules,
                       :authorization_usages], :to => :read
    has_permission_on [:deployable_entity_versions,
                       :builds], to: :reset
    has_permission_on [:impersonation], to: [:impersonate, :stop_it]
  end
end

privileges do
  privilege :manage, :includes => [:create, :read, :update, :delete]
  privilege :read, :includes => [:index, :show]
  privilege :create, :includes => :new
  privilege :update, :includes => :edit
  privilege :delete, :includes => :destroy
  privilege :anon_read
  privilege :anon_write
end

authorization do
  role :guest do
    has_permission_on :welcome, to: :index
    has_permission_on :get_latest, to: :show
    has_permission_on :user_sessions, to: [:new, :create, :destroy]
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
    has_permission_on :api_test_results, to: :create
    has_permission_on :api_repeated_answers, to: :create
  end

  role :user_manager do
    has_permission_on [:users], to: :manage
  end

  role :inquirer do
    includes [:guest, :user_manager]
    has_permission_on [
      :tasks,
      :results,
      :user_tables,
      :keys,
      :lookup_tables
    ], to: :manage
    has_permission_on [
      :help
    ], to: :read
  end

  role :admin do
    includes [:guest, :inquirer, :user_manager]
    has_permission_on [
      :tasks,
      :results,
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
      :user_tables,
      :activities
    ], to: :manage
    has_permission_on [:user_tables], to: :retry_migration
    has_permission_on [:authorization_rules,
                       :authorization_usages], :to => :read
    has_permission_on [:deployable_entity_versions,
                       :builds], to: :reset
    has_permission_on [:impersonation], to: [:impersonate, :stop_it]
    has_permission_on [:test_results, :test_vms, :test_items, :test_item_vms], :to => :read
    has_permission_on [:repeated_answers], :to => :read
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

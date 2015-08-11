authorization do
  role :guest do
    ## ------------------------------------------------------------------
    ## Rules for human users
    ## ------------------------------------------------------------------

    has_permission_on :welcome, to: :index
    has_permission_on :user_sessions, to: [:new, :create, :destroy]
    has_permission_on :users, to: [:toggle_monitoring, :show_current]
    has_permission_on :help, to: :read


    ## ------------------------------------------------------------------
    ## Rules for public APIs which are authenticated through nginx
    ## ------------------------------------------------------------------

    has_permission_on [
      :api_tasks,
      :api_task_results,
      :api_clusters
    ], to: :anon_read

    has_permission_on [
      :api_tasks
    ], to: :anon_write


    ## ------------------------------------------------------------------
    ## Rules for infrastructure APIs which are authenticated through nginx
    ## ------------------------------------------------------------------

    has_permission_on [
      :infrastructure_api_machines,
      :infrastructure_api_cluster_lists,
      :infrastructure_api_authenticated
    ], to: :anon_read
    has_permission_on [
      :infrastructure_api_results,
      :infrastructure_api_audit_logs,
      :infrastructure_api_build_progress,
      :infrastructure_api_machines,
      :infrastructure_api_clusters,
      :infrastructure_api_integration_tests
    ], to: :anon_write
    has_permission_on :infrastructure_api_test_results, to: :create
    has_permission_on :infrastructure_api_repeated_answers, to: :create
    has_permission_on :infrastructure_api_task_codes, to: :create
  end

  role :cluster_manager do
    has_permission_on [
      :user_tables,
      :lookup_tables,
      :users
    ], to: :manage
    has_permission_on [:user_tables], to: [:retry_migration, :clear]
  end

  role :inquirer do
    includes [:guest]
    has_permission_on [
      :tasks,
      :results,
      :keys
    ], to: :manage
    has_permission_on [
      :user_tables,
      :help
    ], to: :read
    has_permission_on [:sandbox], to: :run
    has_permission_on :tasks, to: :delete_results
  end

  role :admin do
    includes [:guest, :inquirer, :cluster_manager]
    has_permission_on [
      :tasks,
      :results,
      :deployable_entities,
      :deployable_entity_versions,
      :builds,
      :clusters,
      :alterations,
      :cloaks,
      :analysts,
      :permissions,
      :user_permissions,
      :metrics,
      :user_tables,
      :activities,
      :audit_logs,
      :capabilities
    ], to: :manage
    has_permission_on [:airpub], to: [:index, :subscribe]
    has_permission_on [:user_tables], to: [:retry_migration, :clear]
    has_permission_on [:authorization_rules,
                       :authorization_usages], :to => :read
    has_permission_on [:deployable_entity_versions,
                       :builds], to: [:reset, :branch_info]
    has_permission_on [:impersonation], to: [:impersonate, :stop_it]
    has_permission_on [:test_results, :test_vms, :test_items, :test_item_vms], :to => :read
    has_permission_on [:repeated_answers], :to => [:read, :update]
    has_permission_on [:repeated_answers], :to => [:update]
    has_permission_on [:ra_task_codes], :to => [:update]
    has_permission_on :audit_logs, :to => [:cluster, :cloak]
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

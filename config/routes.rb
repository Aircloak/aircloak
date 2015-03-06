Web::Application.routes.draw do
  ## ------------------------------------------------------------------
  ## Web frontend for human Aircloak users. No APIs
  ## ------------------------------------------------------------------

  # Deployable entities are the applications we deploy on the cloaks.
  # For example cloak-core, manny-core, etc.
  resources :deployable_entities do
    # Each deployable entity has any number of built versions.
    # We keep build logs, and allow them to be rebuilt if a
    # transient error caused the build to fail
    resources :deployable_entity_versions do
      post 'reset', on: :member
    end
  end
  # Resource for builds of cloak software.
  # A build in this context is a set of
  # deployable entities that together make
  # a cloak machine setup
  resources :builds do
    post 'reset', on: :member
    get 'branch_info', on: :collection
  end
  get 'login' => 'user_sessions#new'
  get 'logout' => 'user_sessions#destroy'
  resources :user_sessions
  resources :users do
    post "toggle_monitoring", on: :member, action: "toggle_monitoring"
  end
  resources :permissions
  resources :cloaks
  resources :analysts
  # Allows aircloak employees to inspect activities performed by
  # users on the web system
  resources :activities
  resources :clusters
  resources :alterations
  resources :test_results, only: [:index, :show]
  resources :test_vms, only: [:show]
  resources :test_items, only: [:show]
  resources :test_item_vms, only: [:show]
  resources :repeated_answers, only: [:index, :show, :update]
  resources :ra_task_codes, only: [:update]
  resources :metrics
  get '/airpub', to: 'airpub#index', as: 'airpub'
  post '/airpub', to: 'airpub#subscribe'
  get "impersonate/:analyst_id", to: "impersonation#impersonate"
  get "i_dont_want_to_be_an_imposter", to: "impersonation#stop_it"
  resources :capabilities
  resources :audit_logs, except: [:create, :destroy] do
    collection do
      get 'cluster/:cluster_id', to: :cluster
      get 'cloak/:cloak_id', to: :cloak
    end
  end


  ## ------------------------------------------------------------------
  ## Web frontend for human external users. No APIs
  ## ------------------------------------------------------------------

  resources :tasks do
    post "execute_as_batch_task", on: :member, action: 'execute_as_batch_task'
    get "latest_results", on: :member, action: 'latest_results'
    get "all_results", on: :member, action: 'all_results'
    post "delete_results", on: :member
  end
  resources :results
  resources :user_tables do
    post "retry_migration", on: :member, action: "retry_migration"
    post "clear", on: :member, action: "clear"
  end
  resources :lookup_tables
  resources :keys
  resources :help
  post "/sandbox/run", to: "sandbox#run"
  root to: 'welcome#index'


  ## ------------------------------------------------------------------
  ## Client API's /api (served from api.aircloak.com)
  ## ------------------------------------------------------------------

  # Resource that very much mimicks the cloaks resource,
  # but makes cloaked machines available in a format
  # consumable by manny-air
  scope "/api", module: :api do
    resources :tasks, path: "tasks" do
      resources :task_results, path: "results"
      post :run, on: :member
      post :subscribe_request, on: :member
    end
  end


  ## ------------------------------------------------------------------
  ## Infrastructure API's /infrastructure-api
  ## (served from infrastructure-api.aircloak.com)
  ## ------------------------------------------------------------------

  # We track the progress of both individual
  # deployable entities being built (and
  # collect the log output for better trouble
  # shooting), and the progress of complete
  # builds.
  scope "/infrastructure-api", module: :infrastructure_api do
    resources :repeated_answers, only: [:create] # Used by cloak's
    resources :task_codes, only: [:create] # Used by cloak's
    resources :results, only: [:create] # Used by cloak's
    resources :audit_logs, only: [:create] # Used by cloak's
    resources :test_results, only: [:create] # Used by TestServer
    post 'register_build_progress' => "build_progress#build_progress" # Used by BuildServer
    post 'register_version_progress' => "build_progress#version_progress" # Used by BuildServer
    get "clusters", to: "cluster_lists#index" # Used by manny-air
    get "clusters/:id", to: "cluster_lists#show" # Used by manny-air
    post "clusters/:id/status", to: "clusters#status" # Used by manny-air
    resources :machines, only: [:index] do
      post 'broken', on: :member # Used by manny-air
      post 'synchronize', on: :member # Used by manny-air
      get 'setup_info', on: :collection # Used by install.sh on cloakinst
    end
  end
end

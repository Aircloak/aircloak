Web::Application.routes.draw do
  ## ------------------------------------------------------------------
  ## Web frontend for human clients. Everything not an API
  ## ------------------------------------------------------------------

  resources :deployable_entities do
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

  resources :test_results, only: [:index, :show]
  resources :test_vms, only: [:show]
  resources :test_items, only: [:show]
  resources :test_item_vms, only: [:show]

  resources :repeated_answers, only: [:index, :show, :update]
  resources :ra_task_codes, only: [:update]

  resources :tasks do
    post "execute_as_batch_task", on: :member, action: 'execute_as_batch_task'
    get "latest_results", on: :member, action: 'latest_results'
    get "all_results", on: :member, action: 'all_results'
  end
  resources :results
  resources :user_tables do
    post "retry_migration", on: :member, action: "retry_migration"
  end
  resources :lookup_tables
  resources :keys
  resources :help

  resources :metrics

  get '/airpub', to: 'airpub#index', as: 'airpub'
  post '/airpub', to: 'airpub#subscribe'

  get "impersonate/:analyst_id", to: "impersonation#impersonate"
  get "i_dont_want_to_be_an_imposter", to: "impersonation#stop_it"

  post "/sandbox/run", to: "sandbox#run"

  resources :capabilities

  resources :audit_logs do
    collection do
      get 'cluster/:cluster_id', to: :cluster
      get 'cloak/:cloak_id', to: :cloak
    end
  end

  root to: 'welcome#index'


  ## ------------------------------------------------------------------
  ## Client API's /api (served from api.aircloak.com)
  ## ------------------------------------------------------------------

  # Resource that very much mimicks the cloaks resource,
  # but makes cloaked machines available in a format
  # consumable by manny-air
  scope "/api" do
    resources :machines, only: [:index] do
      post 'broken', on: :member
      post 'synchronize', on: :member
      get 'setup_info', on: :collection
    end

    resources :api_tasks, path: "tasks" do
      resources :api_task_results, path: "results"
      post "execute_as_batch_task", on: :member, action: 'execute_as_batch_task'
    end

    resources :api_test_results, path: "test_results", only: [:create]

    resources :api_repeated_answers, path: "repeated_answers", only: [:create]
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
  post 'register_build_progress' => "build_progress#build_progress"
  post 'register_version_progress' => "build_progress#version_progress"

  get "/api/clusters", to: "cluster_lists#index"
  get "/api/clusters/:id", to: "cluster_lists#show"
  post "/api/clusters/:id/status", to: "api_clusters#status"
end

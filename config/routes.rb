Web::Application.routes.draw do
  resources :deployable_entities do
    resources :deployable_entity_versions do
      post 'reset', on: :member
    end
  end

  # This action is for receiving information about
  # successful builds from Travis-CI.
  # It will register a commit, so it can
  # later be compiled as part of a build
  post 'register_version' => "register_version#create"

  # Resource for builds of cloak software.
  # A build in this context is a set of
  # deployable entities that together make
  # a cloak machine setup
  resources :builds do
    post 'reset', on: :member
  end

  # We track the progress of both individual
  # deployable entities being built (and
  # collect the log output for better trouble
  # shooting), and the progress of complete
  # builds.
  post 'register_build_progress' => "build_progress#build_progress"
  post 'register_version_progress' => "build_progress#version_progress"

  get 'login' => 'user_sessions#new'
  get 'logout' => 'user_sessions#destroy'
  resources :user_sessions
  resources :users
  resources :permissions

  resources :cloaks
  resources :analysts

  # Resource that very much mimicks the cloaks resource,
  # but makes cloaked machines available in a format
  # consumable by manny-air
  scope "/api" do
    resources :machines, only: [:index] do
      post 'broken', on: :member
      post 'synchronize', on: :member
      get 'setup_info', on: :collection
    end

    resources :api_tasks, path: "tasks", only: [:show] do
      get "latest_result_id", on: :member, action: 'get_latest_result_id'
      get "results/:result", on: :member, action: 'get_result'
      post "execute_as_batch_task", on: :member, action: 'execute_as_batch_task'
    end
  end

  post "/api/version_tests/:id", to: "version_tests#update"
  get "/api/clusters", to: "cluster_lists#index"
  get "/api/clusters/:id", to: "cluster_lists#show"
  post "/api/clusters/:id/status", to: "api_clusters#status"

  unless Rails.configuration.installation.global
    get "/version_tests/create_local", to: "version_tests#create_local"
  end
  resources :version_tests, except: [:create, :update]

  resources :clusters, :except => :destroy

  resources :tasks do
    post "execute_as_batch_task", on: :member, action: 'execute_as_batch_task'
  end
  resources :results
  resources :tables do
    post "retry_migration", on: :member, action: "retry_migration"
  end

  resources :os_tags

  resources :metrics

  root to: 'welcome#index'
end

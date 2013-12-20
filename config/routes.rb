Web::Application.routes.draw do
  resources :deployable_entities do
    resources :deployable_entity_versions
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
  resources :builds

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
  resources :get_latest

  resources :verifications do
    post 'event', on: :collection
    post 'verify', on: :member
  end

  resources :staging_machines

  resources :deployment_groups do
    post 'create_command', on: :member
  end

  resources :client_binaries
  resources :client_file_versions
  resources :client_file_types
  resources :client_files

  resources :commands do
    get 'signed_command', on: :member
  end

  resources :cloaks
  # Resource that very much mimicks the cloaks resource,
  # but makes cloaked machines available in a format
  # consumable by manny-air
  scope "/api" do
    resources :machines, only: [:index] do
      post 'broken', on: :member
      post 'synchronize', on: :member
      get 'setup_info', on: :collection
    end

    resources :cluster_lists, path: "clusters", only: [:index, :show]

    resources :api_queries, path: "queries", only: [:create, :show] do
      get "results/:result", on: :member, action: 'get_result'
      post "execute_as_batch_query", on: :member, action: 'execute_as_batch_query'
    end
  end

  post "/api/version_tests/:id", to: "version_tests#update"
  unless Rails.configuration.installation.global
    get "/version_tests/create_local", to: "version_tests#create_local"
  end
  resources :version_tests, except: [:create, :update]

  resources :clusters, :except => :destroy

  resources :tasks, only: [:index, :edit, :update, :show, :destroy] do
    collection do
      post "update_task_binary"
    end
  end
  resources :queries do
    member do
      post "execute_as_batch_query"
    end
  end
  resources :results

  resources :os_tags

  root to: 'welcome#index'
end

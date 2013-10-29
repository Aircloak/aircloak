Web::Application.routes.draw do
  resources :deployable_entities

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

  resources :queries do
    member do
      post "execute_as_batch_query"
    end
    collection do
      post "upload_query_data"
    end
  end
  resources :results
  resources :properties

  root to: 'welcome#index'
end

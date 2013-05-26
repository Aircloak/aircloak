Web::Application.routes.draw do
  get 'login' => 'user_sessions#new'
  get 'logout' => 'user_sessions#destroy'
  resources :user_sessions
  resources :users
  resources :permissions

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

  post "/queries/upload_query_file", :to => "temp_query_files#create"
  resources :queries do
    member do
      post "execute_as_batch_query"
    end
  end
  resources :results

  root to: 'welcome#index'
end

Web::Application.routes.draw do
  resources :permissions

  get 'login' => 'user_sessions#new'
  get 'logout' => 'user_sessions#destroy'

  resources :user_sessions

  resources :users

  resources :verifications do
    post 'event', on: :collection
    post 'verify', on: :member
  end

  resources :client_file_events

  resources :staging_machines

  resources :deployment_groups do
    post 'create_command', on: :member
  end

  resources :client_file_versions
  resources :client_file_types
  resources :client_files

  resources :commands do
    get 'signed_command', on: :member
  end
  # get "signed_commands", to: "commands#signed_commands"

  resources :client_binaries

  resources :cloaks

  post "/queries/upload_query_file", :to => "temp_query_files#create"
  resources :queries do
    member do
      post "execute_as_batch_query"
    end
  end
  resources :results

  root to: 'welcome#index'

  # Example of regular route:
  #   get 'products/:id' => 'catalog#view'

  # Example of named route that can be invoked with purchase_url(id: product.id)
  #   get 'products/:id/purchase' => 'catalog#purchase', as: :purchase

  # Example resource route (maps HTTP verbs to controller actions automatically):
  #   resources :products

  # Example resource route with options:
  #   resources :products do
  #     member do
  #       get 'short'
  #       post 'toggle'
  #     end
  #
  #     collection do
  #       get 'sold'
  #     end
  #   end

  # Example resource route with sub-resources:
  #   resources :products do
  #     resources :comments, :sales
  #     resource :seller
  #   end

  # Example resource route with more complex sub-resources:
  #   resources :products do
  #     resources :comments
  #     resources :sales do
  #       get 'recent', on: :collection
  #     end
  #   end

  # Example resource route within a namespace:
  #   namespace :admin do
  #     # Directs /admin/products/* to Admin::ProductsController
  #     # (app/controllers/admin/products_controller.rb)
  #     resources :products
  #   end
end

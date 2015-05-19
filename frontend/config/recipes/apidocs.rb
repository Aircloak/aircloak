namespace :apidocs do
  desc "Build assets for REST API docs"
  task :build, roles: :app do
    run "#{release_path}/build_api_docs.sh"
  end
  before "deploy:assets:precompile", "apidocs:build"
end
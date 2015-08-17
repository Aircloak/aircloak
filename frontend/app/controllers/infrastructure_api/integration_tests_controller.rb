require './lib/build_versions_assigner.rb'
require './lib/token_generator.rb'

class InfrastructureApi::IntegrationTestsController < ApplicationController
  filter_access_to :all, require: :anon_write
  protect_from_forgery :except => :create

  # This is sort of a catch all method for handling the requests
  # needed by the erlang backend for performing integration tests
  # on our system
  def create
    payload = JSON.parse(request.raw_post)
    action = payload["action"]
    response = case action
    when "get_analyst_id"
      {success: true, analyst_id: test_analyst.id}
    when "create_build"
      create_build
    when "destroy_build"
      destroy_build payload["build_id"]
    when "create_cluster"
      create_cluster payload["build_id"]
    when "destroy_cluster"
      destroy_cluster payload["cluster_id"]
    when "create_table"
      create_table payload
    when "destroy_table"
      destroy_table payload["table_id"]
    when "create_upload_key"
      create_upload_key
    when "revoke_key"
      revoke_key payload["key_id"]
    when "run_task"
      run_task payload
    when "add_test_result"
      add_test_result payload
    else
      {success: false, description: "Unknown action"}
    end
    render json: response
  end

private

  # We want to create a new build with the latest state of the
  # develop. If one already exists for the same configuration,
  # then we use that instead.
  def create_build
    name = "integration-test-#{Time.now.to_s}"
    build = Build.new(name: name, manual: false)
    BuildVersionsAssigner.assign_from_params build, {"from_develop" => "true"}
    if build.save
      {
        success: true,
        delete_later: true,
        name: name,
        id: build.id,
        build_completed: build.build_completed || false
      }
    else
      if build.errors.include?(:fingerprint) then
        # Find the existing build with the same fingerprint
        # and reuse that
        usable_build = nil
        Build.all.each do |existing_build|
          if existing_build.fingerprint == build.fingerprint then
            usable_build = existing_build
            break
          end
        end
        if usable_build
          {
            success: true,
            delete_later: false,
            build_completed: usable_build.build_completed || true,
            name: usable_build.name,
            id: usable_build.id
          }
        else
          {
            success: false,
            description: "Already exists a build with matching fingerprint. Couldn't find it..."
          }
        end
      else
        {success: false, description: "Couldn't create build: #{format_errors build}"}
      end
    end
  end

  def destroy_build build_id
    build = Build.find(build_id)
    if build.destroy then
      {success: true}
    else
      {success: false, description: "#{format_errors build}"}
    end
  end

  def create_cluster build_id
    cluster = Cluster.new(
      name: "integration-test-cluster. Build #{build_id}, created at #{Time.now}",
      build_id: build_id
    )
    # We need three cloaks for the test cluster
    num_cloaks = Conf.get("/settings/air/integration_test/cluster_size").to_i
    cloaks = Cloak.all_available[0...num_cloaks]
    return {success: false, description: "Not enough cloaks to create cluster"} unless cloaks.size == num_cloaks
    if cluster.assign_analysts([test_analyst]) and cluster.assign_cloaks(cloaks) && cluster.save then
      {success: true, cluster_id: cluster.id, cloaks: cloaks.map(&:ip)}
    else
      {success: false, description: "#{format_errors cluster}"}
    end
  end

  def destroy_cluster cluster_id
    cluster = Cluster.find(cluster_id)
    cluster.initiate_destroy!
    {success: true}
  end

  def format_errors object
    description = "Problem: #{object.errors.full_messages.join(", ")}"
  end

  def create_table payload
    params = {
      table_name: payload["name"],
      cluster_id: payload["cluster_id"],
      migration: payload["migration"],
      table_json: payload["table_json"]
    }
    table = UserTable.from_params(test_analyst.id, params)
    migration = UserTableMigration.from_params(params)
    if Migrator.migrate(table, migration) then
      {success: true, table_id: table.id}
    else
      {success: false, description: "table migration failed"}
    end
  end

  def destroy_table table_id
    table = test_analyst.user_tables.find(table_id)
    migration = UserTableMigration.drop_migration table.table_name
    table.pending_delete = true
    if Migrator.migrate table, migration
      {success: true}
    else
      {success: false, description: "Could not destroy table in cloak"}
    end
  end

  def create_upload_key
    user = test_analyst.users.first
    password = TokenGenerator.generate_random_string_of_at_least_length(10)
    key_name = "integration-test upload key – #{TokenGenerator.generate_random_string_of_at_least_length(10)}"
    key = KeyMaterial.create_from_user user, password, key_name, "data_upload_all"
    {success: true, key_pem: key.pem, password: password, key_id: key.id}
  end

  def revoke_key key_id
    key = KeyMaterial.find key_id
    test_analyst.revoke_key key
    {success: true}
  end

  def run_task payload
    cluster = test_analyst.clusters.find(payload["cluster_id"])
    task = test_user.tasks.new(
      name: "Integration-test - #{Time.now.to_i}+#{rand(1000)}",
      cluster: cluster,
      analyst: test_analyst,
      sandbox_type: "lua",
      stored_task: false, # true for streaming or periodic tasks
      update_task: false,
      code_timestamp: Time.now,
      code: payload["code"],
      prefetch: payload["prefetch"],
      one_off: true
    )

    if task.save
      pending_result = task.execute_batch_task
      {success: true, channel_name: pending_result.channel_name}
    else
      {success: false, description: "Could not save task"}
    end
  end

  def add_test_result payload
    IntegrationTest.add_result_for_test(payload["identifier"], payload["result"])
    {success: true}
  end

  def test_analyst
    Analyst.find_by_name("Aircloak - Integration tests")
  end

  def test_user
    test_analyst.users.find_by_login("infrastructure-test")
  end
end

require './lib/migrator'

namespace :air do
  # Specification of test tables. Add new entries here if you want some
  # additional tables.
  TestTables = {
    age: {
      columns: [{name: "age", constraints: [], type: "integer"}],
      generate: lambda {|user_id| {age: rand(50) + 20}}
    },
    height: {
      columns: [{name: "height", constraints: [], type: "integer"}],
      generate: lambda {|user_id| {height: rand(60) + 150}}
    }
  }

  desc "Recreates test tables for users, and inserts dummy users into the database"
  task :recreate_test_users, [:analyst_id, :cluster_id, :num_users] => :environment do |t, args|
    if args.count != 3 || args[:analyst_id].nil? || args[:cluster_id].nil? || args[:num_users].nil?
      abort <<-eos.gsub(/^ +/, '')

        bundle exec rake air:recreate_test_users[analyst_id,cluster_id,num_users]

        Example: bundle exec rake air:recreate_test_users[1,1,1000]
        Note: don't use spaces inside square brackets

      eos
    end

    srand
    analyst_id = args[:analyst_id].to_i
    cluster_id = args[:cluster_id].to_i
    num_users = args[:num_users].to_i

    drop_tables(analyst_id, cluster_id)
    create_tables(analyst_id, cluster_id)
    create_users(analyst_id, cluster_id, num_users)
  end

  task :task_load_test, [:num_iterations] => :environment do |t, args|
    if args.count != 1 || args[:num_iterations].nil?
      abort <<-eos.gsub(/^ +/, '')

        bundle exec rake air:task_load_test[num_iterations]

        Example: bundle exec rake air:task_load_test[1000]

      eos
    end
    tasks = Task.all.to_a
    num_iterations = args[:num_iterations].to_i
    (1..num_iterations).each do |iteration|
      print "\r#{(100.0*iteration/num_iterations).round} %"
      tasks.each do |task|
        task.execute_batch_task
      end
    end
  end

  private
    def drop_tables(analyst_id, cluster_id)
      TestTables.keys.each do |table_name|
        table = user_table(analyst_id, cluster_id, table_name)
        return if table.nil?

        if Migrator.migrate table, UserTableMigration.drop_migration(table_name)
          puts "Dropped table #{table_name}"
        else
          # We don't stop on drop error, since it probably fails if the
          # table is not on the cloak.
          puts "Warning: error dropping table #{table_name}"
        end
      end
    end

    def create_tables(analyst_id, cluster_id)
      TestTables.each do |table_name, table_spec|
        table = user_table(analyst_id, cluster_id, table_name)
        unless table
          params = {
            cluster_id: cluster_id,
            table_name: table_name
          }
          table = UserTable.from_params analyst_id, params
        end

        migration = UserTableMigration.from_params(
          table_json: table_spec[:columns].to_json,
          migration: {
            table_name: table_name,
            action: "create",
            columns: table_spec[:columns]
          }.to_json
        )

        if Migrator.migrate table, migration
          puts "Created table #{table_name}"
        else
          raise "Error creating table #{table_name}"
        end
      end
    end

    def user_table(analyst_id, cluster_id, table_name)
      tables = UserTable.where(analyst_id: analyst_id, cluster_id: cluster_id, table_name: table_name)
      tables.length == 1 ? tables[0] : nil
    end

    def create_users(analyst_id, cluster_id, num_users)
      cluster = Cluster.find(cluster_id)
      analyst = Analyst.find(analyst_id)
      (1..num_users).each do |index|
        print "\rInserting users (#{(100.0*index/num_users).round} %)"
        STDOUT.flush

        user_id = "user_#{index}"
        insert_data = TestTables.keys.inject({}) do |memo, table_name|
          memo.merge(table_name => [TestTables[table_name][:generate].call(user_id)])
        end
        JsonSender.request :post, :no_auth, analyst, cluster, "insert", {user: user_id}, insert_data.to_json
      end
      puts "\nDone!\n\n"
    end
end

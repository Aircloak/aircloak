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
  task :recreate_test_users, [:cluster_id, :num_users] => :environment do |t, args|
    if args.count != 2 || args[:cluster_id].nil? || args[:num_users].nil?
      abort <<-eos.gsub(/^ +/, '')

        bundle exec rake air:recreate_test_users[cluster_id,num_users]

        Example: bundle exec rake air:recreate_test_users[1,1000]
        Note: don't use spaces inside square brackets

      eos
    end

    srand
    drop_tables(args[:cluster_id].to_i)
    create_tables(args[:cluster_id].to_i)
    create_users(args[:cluster_id].to_i, args[:num_users].to_i)
  end

  private
    def drop_tables(cluster_id)
      TestTables.keys.each do |table_name|
        table = analyst_table(cluster_id, table_name)
        return if table.nil?

        if Migrator.migrate table, AnalystTableMigration.drop_migration(table_name)
          puts "Dropped table #{table_name}"
        else
          # We don't stop on drop error, since it probably fails if the
          # table is not on the cloak.
          puts "Warning: error dropping table #{table_name}"
        end
      end
    end

    def create_tables(cluster_id)
      TestTables.each do |table_name, table_spec|
        table =
          analyst_table(cluster_id, table_name) ||
          AnalystTable.from_params(cluster_id: cluster_id, table_name: table_name)

        migration = AnalystTableMigration.from_params(
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

    def analyst_table(cluster_id, table_name)
      tables = AnalystTable.where(cluster_id: cluster_id, table_name: table_name)
      tables.length == 1 ? tables[0] : nil
    end

    def create_users(cluster_id, num_users)
      cluster = Cluster.find(cluster_id)
      (1..num_users).each do |index|
        print "\rInserting users (#{(100.0*index/num_users).round} %)"
        STDOUT.flush

        user_id = "user_#{index}"
        insert_data = TestTables.keys.inject({}) do |memo, table_name|
          memo.merge(table_name => [TestTables[table_name][:generate].call(user_id)])
        end
        JsonSender.post cluster, "insert", insert_data.to_json, user: user_id
      end
      puts "\nDone!\n\n"
    end
end

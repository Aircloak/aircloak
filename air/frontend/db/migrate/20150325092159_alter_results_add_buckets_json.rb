class AlterResultsAddBucketsJson < ActiveRecord::Migration
  def up
    add_column :results, :buckets_json, :text
    Result.all.each do |result|
      # We use plain SQL fetching, since association and Bucket class
      # is later deleted
      buckets = ActiveRecord::Base.connection.select_all("
            SELECT label, value, count, null
            FROM buckets
            WHERE result_id=#{result.id}
          ")

      result.buckets_json = buckets.map do |bucket|
        {
          label: bucket["label"],
          value: bucket["value"],
          count: bucket["count"] ? bucket["count"].to_i : nil
        }
      end.to_json

      unless result.save
        raise ArgumentError.new("Error converting buckets for results id=#{result.id}")
      end
    end
  end

  def down
    remove_column :results, :buckets_json
  end
end

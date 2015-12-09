class RenameHistogramsToPostProcessed < ActiveRecord::Migration
  def change
    rename_column :results, :histograms_json, :post_processed_json
    # convert existing data to new format
    Result.all.each do |result|
      if result.post_processed_json
        result.post_processed_json = "{\"histograms\":" + result.post_processed_json + "}"
        result.save!
      end
    end
  end
end

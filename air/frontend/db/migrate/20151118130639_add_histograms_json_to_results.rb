class AddHistogramsJsonToResults < ActiveRecord::Migration
  def change
    add_column :results, :histograms_json, :text
  end
end

class AddQueryDataToQueries < ActiveRecord::Migration
  def change
    add_column :queries, :packaged_data, :binary
    add_column :queries, :main_package, :string
  end
end

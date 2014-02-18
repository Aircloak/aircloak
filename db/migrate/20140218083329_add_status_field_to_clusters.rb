class AddStatusFieldToClusters < ActiveRecord::Migration
  def change
    add_column :clusters, :status_value, :integer, default: 1
    add_column :clusters, :status_description, :string
  end
end

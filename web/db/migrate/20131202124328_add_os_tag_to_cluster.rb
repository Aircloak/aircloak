class AddOsTagToCluster < ActiveRecord::Migration
  def change
    add_reference :clusters, :os_tag, index: true
  end
end

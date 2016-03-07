class RemoveOsTagFromClusters < ActiveRecord::Migration
  def change
    remove_reference :clusters, :os_tag, index: true
  end
end

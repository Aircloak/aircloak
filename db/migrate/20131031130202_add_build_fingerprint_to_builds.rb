class AddBuildFingerprintToBuilds < ActiveRecord::Migration
  def change
    add_column :builds, :fingerprint, :string
  end
end

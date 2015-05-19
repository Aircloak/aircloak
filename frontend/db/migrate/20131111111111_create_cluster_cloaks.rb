class CreateClusterCloaks < ActiveRecord::Migration
  def change
    create_table :cluster_cloaks do |t|
      t.belongs_to :cluster
      t.belongs_to :cloak

      t.datetime :appointment_date
      t.timestamps
    end
  end
end

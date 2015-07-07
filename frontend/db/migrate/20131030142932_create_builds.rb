class CreateBuilds < ActiveRecord::Migration
  def change
    create_table :builds do |t|
      t.boolean :tpm, default: true
      t.string :name

      t.timestamps
    end
  end
end

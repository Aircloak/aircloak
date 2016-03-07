class CreateIntegrationTests < ActiveRecord::Migration
  def change
    create_table :integration_tests do |t|
      t.string :name
      t.string :identifier

      t.timestamps
    end
  end
end

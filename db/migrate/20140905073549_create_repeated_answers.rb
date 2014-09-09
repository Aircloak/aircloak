class CreateRepeatedAnswers < ActiveRecord::Migration
  def change
    create_table :repeated_answers do |t|
      t.belongs_to :analyst, index: true

      t.string :bucket_label
      t.string :bucket_value
      t.integer :bucket_count
      t.integer :timestamp
      t.string :source_ip
      t.float :noise_sd

      t.timestamps
    end
  end
end

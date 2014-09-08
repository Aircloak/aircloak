class CreateRepeatedAnswers < ActiveRecord::Migration
  def change
    create_table :repeated_answers do |t|
      t.belongs_to :analyst, index: true

      t.string :bucket_label
      t.string :bucket_value
      t.integer :bucket_count
      t.integer :timestamp
      t.string :source_ip
      t.integer :anonparam_k1
      t.float :anonparam_delta_k2
      t.integer :anonparam_k2
      t.float :anonparam_target_error
      t.float :anonparam_sigma
      t.float :anonparam_constant_noise_sd

      t.timestamps
    end
  end
end

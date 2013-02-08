class IndexQueryFile < ActiveRecord::Base
  belongs_to :query_file
  belongs_to :index

  accepts_nested_attributes_for :index

  # If this was the last query file maintaining the
  # index, then we also need to destroy the index
  # itself, as it isn't maintained anymore
  after_destroy do
    # Deleted a query_file
    index.destroy if index.query_files.count < 2
  end
end

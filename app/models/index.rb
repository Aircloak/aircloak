class Index < ActiveRecord::Base
  has_many :index_query_files, :dependent => :destroy
  has_many :query_files, :through => :index_query_files

  def self.from_json i
    index = nil
    if i["fresh"]
      index = Index.new(
        name: i["name"],
        human_name: i["human_name"],
        system_index: !i["system_index"])
      index.save
    else
      index = Index.find(i["id"])
    end
    index
  end
end

## Generated from aircloak/cloak/query.proto for aircloak
require "beefcake"


class QueryBinary
  include Beefcake::Message


  required :package, :string, 1
  required :data, :bytes, 2

end

class CQuery
  include Beefcake::Message

  module Type
    MUTATOR = 0
    READER = 1
  end
  module QueryClass
    BATCH = 0
    STORED = 1
  end

  class BatchOptions
    include Beefcake::Message


    optional :url, :string, 1

  end

  class StoredOptions
    include Beefcake::Message


    required :payload_identifier, :string, 1

  end

  repeated :data, QueryBinary, 1
  required :main_class, :string, 2
  required :type, CQuery::Type, 3
  required :query_id, :fixed64, 4
  required :index, :string, 5
  required :analyst_id, :string, 6
  required :system_query, :bool, 7
  optional :query_class, CQuery::QueryClass, 8
  optional :batch_options, CQuery::BatchOptions, 9
  optional :stored_options, CQuery::StoredOptions, 10

end

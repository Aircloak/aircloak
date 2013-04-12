# encoding: UTF-8
# This file is auto-generated from the current state of the database. Instead
# of editing this file, please use the migrations feature of Active Record to
# incrementally modify your database, and then regenerate this schema definition.
#
# Note that this schema.rb definition is the authoritative source for your
# database schema. If you need to create the application database on another
# system, you should be using db:schema:load, not running all the migrations
# from scratch. The latter is a flawed and unsustainable approach (the more migrations
# you'll amass, the slower it'll run and the greater likelihood for issues).
#
# It's strongly recommended that you check this file into your version control system.

ActiveRecord::Schema.define(version: 20130412075321) do

  create_table "client_binaries", force: true do |t|
    t.boolean  "updater",          default: false
    t.integer  "size"
    t.string   "sha1"
    t.binary   "data"
    t.integer  "times_downloaded", default: 0
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  create_table "client_file_types", force: true do |t|
    t.string   "name"
    t.datetime "created_at"
    t.datetime "updated_at"
    t.string   "human_name"
  end

  create_table "client_file_versions", force: true do |t|
    t.binary   "data"
    t.string   "sha1"
    t.integer  "size"
    t.integer  "times_downloaded"
    t.integer  "client_file_id"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  create_table "client_files", force: true do |t|
    t.string   "name"
    t.string   "local_name"
    t.integer  "client_file_type_id"
    t.datetime "created_at"
    t.datetime "updated_at"
    t.string   "extension"
  end

  create_table "cloaks", force: true do |t|
    t.string   "name"
    t.string   "ip"
    t.boolean  "part_of_ring", default: false
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  create_table "commands", force: true do |t|
    t.binary   "command_binary"
    t.boolean  "valid_command",    default: false
    t.integer  "times_downloaded", default: 0
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  create_table "exception_results", force: true do |t|
    t.integer  "query_id"
    t.string   "stack",      array: true
    t.integer  "count"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  add_index "exception_results", ["count"], name: "index_exception_results_on_count"
  add_index "exception_results", ["query_id"], name: "index_exception_results_on_query_id"

  create_table "index_query_files", force: true do |t|
    t.integer  "index_id"
    t.integer  "query_file_id"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  create_table "indices", force: true do |t|
    t.string   "name",         default: ""
    t.string   "human_name",   default: ""
    t.boolean  "system_index", default: false
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  create_table "pending_results", force: true do |t|
    t.integer  "query_id"
    t.string   "auth_token"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  add_index "pending_results", ["query_id"], name: "index_pending_results_on_query_id"

  create_table "percentile_results", force: true do |t|
    t.string   "bucket"
    t.integer  "query_id"
    t.hstore   "raw_values"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  add_index "percentile_results", ["bucket"], name: "index_percentile_results_on_bucket"
  add_index "percentile_results", ["query_id"], name: "index_percentile_results_on_query_id"

  create_table "properties_results", force: true do |t|
    t.string   "bucket"
    t.boolean  "numeric",    default: false
    t.string   "str_value"
    t.integer  "long_value"
    t.integer  "count"
    t.integer  "query_id"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  add_index "properties_results", ["bucket"], name: "index_properties_results_on_bucket"
  add_index "properties_results", ["count"], name: "index_properties_results_on_count"
  add_index "properties_results", ["long_value"], name: "index_properties_results_on_long_value"
  add_index "properties_results", ["numeric"], name: "index_properties_results_on_numeric"
  add_index "properties_results", ["query_id"], name: "index_properties_results_on_query_id"
  add_index "properties_results", ["str_value"], name: "index_properties_results_on_str_value"

  create_table "queries", force: true do |t|
    t.string   "name"
    t.integer  "index_id"
    t.boolean  "update_query", default: false
    t.string   "identifier"
    t.boolean  "system_query", default: false
    t.boolean  "mutator",      default: false
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  create_table "query_files", force: true do |t|
    t.string   "sha"
    t.string   "name"
    t.integer  "query_id"
    t.binary   "data"
    t.boolean  "query_interface"
    t.boolean  "index_ops"
    t.string   "package"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  create_table "temp_query_files", force: true do |t|
    t.binary   "data"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

end

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

ActiveRecord::Schema.define(version: 20131025122504) do

  create_table "client_binaries", force: true do |t|
    t.boolean  "updater",          default: false
    t.integer  "size"
    t.string   "sha1"
    t.binary   "data"
    t.integer  "times_downloaded", default: 0
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  create_table "client_file_events", force: true do |t|
    t.boolean  "positive",               default: true
    t.string   "description"
    t.string   "event"
    t.integer  "client_file_version_id"
    t.integer  "staging_machine_id"
    t.datetime "created_at"
    t.datetime "updated_at"
    t.integer  "client_file_id"
  end

  add_index "client_file_events", ["client_file_id"], name: "index_client_file_events_on_client_file_id"

  create_table "client_file_types", force: true do |t|
    t.string   "name"
    t.string   "extension"
    t.string   "human_name"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  create_table "client_file_versions", force: true do |t|
    t.binary   "data"
    t.string   "sha1"
    t.integer  "size"
    t.integer  "times_downloaded"
    t.integer  "client_file_id"
    t.boolean  "verified",         default: false
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  create_table "client_files", force: true do |t|
    t.string   "name"
    t.string   "local_name"
    t.integer  "client_file_type_id"
    t.datetime "created_at"
    t.datetime "updated_at"
    t.boolean  "requires_verifications", default: true
  end

  create_table "cloaks", force: true do |t|
    t.string   "name"
    t.string   "ip"
    t.boolean  "part_of_ring", default: false
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  create_table "command_file_versions", force: true do |t|
    t.integer  "command_id"
    t.integer  "client_file_version_id"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  add_index "command_file_versions", ["client_file_version_id"], name: "index_command_file_versions_on_client_file_version_id"
  add_index "command_file_versions", ["command_id"], name: "index_command_file_versions_on_command_id"

  create_table "commands", force: true do |t|
    t.binary   "command_binary"
    t.boolean  "valid_command",       default: false
    t.integer  "times_downloaded",    default: 0
    t.datetime "created_at"
    t.datetime "updated_at"
    t.integer  "deployment_group_id"
  end

  add_index "commands", ["deployment_group_id"], name: "index_commands_on_deployment_group_id"

  create_table "deployment_groups", force: true do |t|
    t.string   "identifier"
    t.string   "name"
    t.boolean  "verified_only", default: true
    t.boolean  "autoupdate",    default: false
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  create_table "exception_results", force: true do |t|
    t.integer  "query_id"
    t.string   "stack"
    t.integer  "count"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  add_index "exception_results", ["count"], name: "index_exception_results_on_count"
  add_index "exception_results", ["query_id"], name: "index_exception_results_on_query_id"

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

  create_table "permissions", force: true do |t|
    t.string   "name"
    t.string   "description"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  create_table "properties", force: true do |t|
    t.integer  "query_id"
    t.string   "property"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  add_index "properties", ["query_id"], name: "index_properties_on_query_id"

  create_table "property_result_counts", force: true do |t|
    t.integer  "property_result_id"
    t.integer  "count"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  create_table "property_results", force: true do |t|
    t.boolean  "numeric",     default: false
    t.string   "str_value"
    t.integer  "long_value"
    t.datetime "created_at"
    t.datetime "updated_at"
    t.integer  "property_id"
  end

  add_index "property_results", ["long_value"], name: "index_property_results_on_long_value"
  add_index "property_results", ["numeric"], name: "index_property_results_on_numeric"
  add_index "property_results", ["str_value"], name: "index_property_results_on_str_value"

  create_table "queries", force: true do |t|
    t.string   "name"
    t.integer  "index_id"
    t.boolean  "update_query",    default: false
    t.string   "identifier"
    t.boolean  "system_query",    default: false
    t.boolean  "mutator",         default: false
    t.binary   "packaged_data"
    t.string   "main_package"
    t.boolean  "manages_indices", default: false
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  create_table "query_indices", force: true do |t|
    t.integer  "query_id"
    t.integer  "user_id"
    t.string   "name"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  add_index "query_indices", ["query_id"], name: "index_query_indices_on_query_id"
  add_index "query_indices", ["user_id"], name: "index_query_indices_on_user_id"

  create_table "sessions", force: true do |t|
    t.string   "session_id"
    t.text     "data"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  add_index "sessions", ["session_id"], name: "index_sessions_on_session_id"
  add_index "sessions", ["updated_at"], name: "index_sessions_on_updated_at"

  create_table "staging_machines", force: true do |t|
    t.string   "name"
    t.string   "description"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  create_table "user_permissions", force: true do |t|
    t.integer  "user_id"
    t.integer  "permission_id"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  add_index "user_permissions", ["permission_id"], name: "index_user_permissions_on_permission_id"
  add_index "user_permissions", ["user_id"], name: "index_user_permissions_on_user_id"

  create_table "users", force: true do |t|
    t.string   "login"
    t.string   "email"
    t.string   "crypted_password"
    t.string   "password_salt"
    t.string   "persistence_token"
    t.string   "single_access_token"
    t.string   "perishable_token"
    t.integer  "login_count"
    t.integer  "failed_login_count"
    t.datetime "last_request_at"
    t.datetime "current_login_at"
    t.datetime "last_login_at"
    t.string   "current_login_ip"
    t.string   "last_login_ip"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  add_index "users", ["email"], name: "index_users_on_email"
  add_index "users", ["last_request_at"], name: "index_users_on_last_request_at"
  add_index "users", ["login"], name: "index_users_on_login"
  add_index "users", ["persistence_token"], name: "index_users_on_persistence_token"

end

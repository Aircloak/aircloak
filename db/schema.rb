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

ActiveRecord::Schema.define(version: 20130207131407) do

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

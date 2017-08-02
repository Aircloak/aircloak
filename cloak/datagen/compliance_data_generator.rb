# This is a data generator that generates a small test dataset that can be imported
# into all our supported datasources. The goal is to have a small dataset that is
# very quick to query, where we can easily verify that it produces the same results
# across all datasources and datasource types.
#
# The generated data model is as follows:
# - users: containing some per user data
# - notes: a user has many notes, and a note belongs to a user
# - drafts_changes: a draft belong to a note
# - addresses: a user has a address entry

require 'openssl'
require 'json'
require 'base64'
require 'json'
require 'date'
require 'csv'
require 'optparse'


# -------------------------------------------------------------------
# Config parameters
# -------------------------------------------------------------------

options = {}
OptionParser.new do |opts|
  opts.banner = "Usage: #{File.basename($PROGRAM_NAME)} [options]"

  opts.on("--users N", OptionParser::DecimalInteger,
    "Set number of users. Data size will be bigger for more users.") do |users|
      options[:users] = users
    end
end.parse!

$user_ids = (1..options[:users])
$encryption_key = "1234567890ABCDEF"
$iv = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0].pack("C*")


# -------------------------------------------------------------------
# Available data
# -------------------------------------------------------------------

names = [
  "Anna", "Anna", "Anna", "Anna", "Anna", "Anna", "Anna", "Anna", "Anna", "Bertha", "Elisabeth", "Emma", "Finn",
  "Frieda" ", Friedrich", "Friedrich", "Friedrich", "Friedrich", "Gertrud", "Hannah", "Hans", "Hans", "Hans",
  "Hans", "Hans", "Hans", "Heinrich", "Hermann", "Jan", "Jan", "Jan", "Jan", "Jan", "Jan", "Jan", "Jan", "Jan",
  "Jan", "Jan", "Jan", "Jan", "Jannik", "Jonas", "Julia", "Karl", "Lara", "Laura", "Lea", "Lena", "Leon", "Lisa",
  "Luca", "Lukas", "Margarethe", "Maria", "Marie", "Martha", "Michelle", "Niklas", "Otto", "Paul", "Sarah",
  "Tim", "Tim", "Tim", "Tim", "Tim", "Tim", "Tim", "Tom", "Walter", "Wilhelm"
]

words = [
  "abandon", "ability", "able", "abortion", "about", "above", "abroad", "absence", "absolute", "absolutely",
  "absorb", "abuse", "academic", "accept", "access", "accident", "accompany", "accomplish", "according",
  "account", "accurate", "accuse", "achieve", "achievement", "acid", "acknowledge", "acquire", "across",
  "act", "action", "active", "activist", "activity", "actor", "actress", "actual", "actually", "ad",
  "adapt", "add", "addition", "additional", "address", "adequate", "adjust", "adjustment", "administration",
  "administrator", "admire", "admission", "admit", "adolescent", "adopt", "adult", "advance", "advanced",
  "advantage", "adventure", "advertising", "advice", "advise", "adviser", "advocate", "affair", "affect",
  "afford", "afraid", "African", "African-American", "after", "afternoon", "again", "against", "age", "agency",
  "agenda", "agent", "aggressive", "ago", "agree", "agreement", "agricultural", "ahead", "aid", "aide",
  "air", "aircraft", "airline", "airport", "album", "alcohol", "alive", "all", "alliance", "allow", "ally",
  "almost", "alone", "along", "already", "also", "alter", "alternative", "although", "always", "AM", "amazing",
  "American", "among", "amount", "analysis", "analyst", "analyze", "ancient", "and", "anger", "angle", "angry",
  "animal", "animal", "animal", "animal", "animal", "animal", "animal", "animal", "animal", "animal", "animal",
  "anniversary", "announce", "announce", "announce", "announce", "announce", "announce", "announce",
  "announce", "announce", "announce", "annual", "another", "answer", "answer", "answer", "answer",
  "answer", "anticipate"
]

cities = [
  "Shanghai", "Shanghai", "Shanghai", "Shanghai", "Karachi", "Beijing", "Delhi", "Lagos",
  "Dhaka", "Guangzhou", "Istanbul", "Tokyo", "Mumbai", "Mumbai", "Mumbai", "Mumbai", "Mumbai"
]


# -------------------------------------------------------------------
# Global state
# -------------------------------------------------------------------

# Get repeatable results
srand(1234)


# -------------------------------------------------------------------
# Utility functions
# -------------------------------------------------------------------

def encrypted(data)
  cipher = OpenSSL::Cipher.new('AES-128-CBC')
  cipher.encrypt
  cipher.key = $encryption_key
  cipher.iv = $iv

  encode64(cipher.update(data) + cipher.final)
end

def encode64(data)
  Base64.strict_encode64(data).chomp
end

def encryption_wrap_for_mongo(data)
  {"$binary" => data, "$type" => "00"}
end

def rand_datetime_as_text()
  epoch = 1500000000 + rand(100026704)
  DateTime.strptime(epoch.to_s,'%s').to_s
end


# -------------------------------------------------------------------
# Data generation - users
# -------------------------------------------------------------------

users = []
$user_ids.each do |user_n|
  name = names.sample
  age = rand(100) + 1
  active = [true, false].sample
  users << {
    id: user_n,
    active: active,
    name: name,
    age: age,
    height: rand() * 100 + 100.0,
  }
end

puts <<EOS
# -------------------------------------------------------------------
# Users data needs the following schemas:
# -------------------------------------------------------------------

Raw data:
CREATE TABLE users (id integer, name text, age integer, active boolean, height real);

Encoded data:
CREATE TABLE users_encoded (id integer, name text, age text, active text, height text);


EOS

CSV.open("output/users.csv", "wb") do |csv|
  csv << ["id", "name", "age", "active", "height"]
  users.each do |entry|
    csv << [entry[:id], entry[:name], entry[:age], entry[:active], entry[:height]]
  end
end

CSV.open("output/users_encoded.csv", "wb") do |csv|
  csv << ["id", "name", "age", "active", "height"]
  users.each do |entry|
    csv << [entry[:id], encrypted(entry[:name]), entry[:age].to_s, entry[:active].to_s, entry[:height].to_s]
  end
end

open("output/users_mongo.json", "w") do |file|
  users.each do |user|
    file.puts user.to_json
  end
end

open("output/users_mongo_encoded.json", "w") do |file|
  users.each do |user|
    user[:name] = encryption_wrap_for_mongo(encrypted(user[:name]))
    user[:age] = user[:age].to_s
    user[:height] = user[:height].to_s
    user[:active] = user[:active].to_s
    file.puts user.to_json
  end
end


# -------------------------------------------------------------------
# Data generation - notes
# -------------------------------------------------------------------

notes = []
note_id = 0
$user_ids.each do |user_n|
  rand(10).times do
    note_id += 1
    notes << {
      id: note_id,
      user_id: user_n,
      title: (1..(rand(5)+1)).to_a.map{words.sample}.join(" "),
      content: (1..(rand(40)+40)).to_a.map{words.sample}.join(" ")
    }
  end
end

puts <<EOS
# -------------------------------------------------------------------
# Notes data needs the following schemas:
# -------------------------------------------------------------------

Raw data:
CREATE TABLE notes (id integer, user_id integer, title text, content text);

Encoded data:
CREATE TABLE notes_encoded (id integer, user_id integer, title text, content text);


EOS

CSV.open("output/notes.csv", "wb") do |csv|
  csv << ["id", "user_id", "title", "content"]
  notes.each do |entry|
    csv << [entry[:id], entry[:user_id], entry[:title], entry[:content]]
  end
end

CSV.open("output/notes_encoded.csv", "wb") do |csv|
  csv << ["id", "user_id", "title", "content"]
  notes.each do |entry|
    csv << [entry[:id], entry[:user_id], encrypted(entry[:title]), encrypted(entry[:content])]
  end
end

open("output/notes_mongo.json", "w") do |file|
  notes.each do |note|
    file.puts note.to_json
  end
end

open("output/notes_mongo_encoded.json", "w") do |file|
  notes.each do |note|
    note[:title] = encryption_wrap_for_mongo(encrypted(note[:title]))
    note[:content] = encryption_wrap_for_mongo(encrypted(note[:content]))
    file.puts note.to_json
  end
end


# -------------------------------------------------------------------
# Data generation - draft
# -------------------------------------------------------------------

drafts = []
draft_id = 0
note_id.times do |note_id|
  changes = []
  rand(5).times do
    changes << {
      date: rand_datetime_as_text(),
      change: (1..(rand(10)+1)).to_a.map{words.sample}.join(" ")
    }
  end

  if changes.length > 0 then
    draft_id += 1
    drafts << {
      id: draft_id,
      note_id: note_id,
      changes: changes
    }
  end
end

puts <<EOS
# -------------------------------------------------------------------
# Drafts data needs the following schemas:
# -------------------------------------------------------------------

Raw data:
CREATE TABLE drafts_changes (id integer, note_id integer, \"changes.date\" timestamp, \"changes.change\" text);

Encoded data:
CREATE TABLE drafts_changes_encoded (id integer, note_id integer, \"changes.date\" text, \"changes.change\" text);


EOS

CSV.open("output/drafts_changes.csv", "wb") do |csv|
  csv << ["id", "note_id", "changes.date", "changes.change"]

  # We generate new unique ID's here, since the ones we have are only
  # MongoDB friendly
  draft_id = 0
  drafts.each do |entry|
    entry[:changes].each do |change|
      draft_id += 1
      csv << [draft_id, entry[:note_id], change[:date], change[:change]]
    end
  end
end

CSV.open("output/drafts_changes_encoded.csv", "wb") do |csv|
  csv << ["id", "note_id", "changes.date", "changes.change"]

  # We generate new unique ID's here, since the ones we have are only
  # MongoDB friendly
  draft_id = 0
  drafts.each do |entry|
    entry[:changes].each do |change|
      draft_id += 1
      csv << [draft_id, entry[:note_id], change[:date], encrypted(change[:change])]
    end
  end
end


open("output/drafts_mongo.json", "w") do |file|
  drafts.each do |draft|
    file.puts draft.to_json
  end
end

open("output/drafts_mongo_encoded.json", "w") do |file|
  drafts.each do |draft|
    draft[:changes] = draft[:changes].map do |draft|
      draft[:change] = encryption_wrap_for_mongo(encrypted(draft[:change]))
      draft
    end
    file.puts draft.to_json
  end
end


# -------------------------------------------------------------------
# Data generation - address
# -------------------------------------------------------------------

addresses = []
$user_ids.each do |user_id|
  addresses << {
    id: user_id,
    user_id: user_id,
    home: {
      city: cities.sample,
      postal_code: rand(89999) + 10000
    },
    work: {
      city: cities.sample,
      postal_code: rand(89999) + 10000
    }
  }
end

puts <<EOS
# -------------------------------------------------------------------
# Address data needs the following schemas:
# -------------------------------------------------------------------

Raw data:
CREATE TABLE addresses (id integer, user_id integer, \"home.city\" text, \"home.postal_code\" integer, \"work.city" text, \"work.postal_code\" integer);

Encoded data:
CREATE TABLE addresses_encoded (id integer, user_id integer, \"home.city\" text, \"home.postal_code\" text, \"work.city" text, \"work.postal_code\" text);


EOS

CSV.open("output/addresses.csv", "wb") do |csv|
  csv << ["id", "user_id", "home.city", "home.postal_code", "work.city", "work.postal_code"]
  addresses.each do |entry|
    csv << [entry[:id], entry[:user_id], entry[:home][:city], entry[:home][:postal_code],
      entry[:work][:city], entry[:work][:postal_code]]
  end
end

CSV.open("output/addresses_encoded.csv", "wb") do |csv|
  csv << ["id", "user_id", "home.city", "home.postal_code", "work.city", "work.postal_code"]
  addresses.each do |entry|
    csv << [entry[:id], entry[:user_id], encrypted(entry[:home][:city]),
      encode64(entry[:home][:postal_code].to_s), encrypted(entry[:work][:city]),
      encode64(entry[:work][:postal_code].to_s)]
  end
end

open("output/addresses_mongo.json", "w") do |file|
  addresses.each do |address|
    file.puts address.to_json
  end
end

open("output/addresses_mongo_encoded.json", "w") do |file|
  addresses.each do |entry|
    entry[:home][:city] = encryption_wrap_for_mongo(encrypted(entry[:home][:city]))
    entry[:work][:city] = encryption_wrap_for_mongo(encrypted(entry[:work][:city]))
    entry[:home][:postal_code] = encode64(entry[:home][:postal_code].to_s)
    entry[:work][:postal_code] = encode64(entry[:work][:postal_code].to_s)
    file.puts entry.to_json
  end
end

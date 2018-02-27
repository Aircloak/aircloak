defmodule Air.Performance.Queries do
  @moduledoc "Queries to run during performance testing"

  @type plain_query :: String.t
  @type differentiated_query :: %{db: String.t, cloak: String.t}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns a list of performance test queries"
  @spec queries() :: [plain_query | differentiated_query]
  def queries(), do: [
    "SELECT * FROM addresses",
    "SELECT * FROM users",
    "SELECT active, age, height FROM users",
    "SELECT active, age, height FROM users WHERE name LIKE 'An%'",
    "SELECT count(*) FROM addresses",
    "SELECT count(*) FROM notes",
    "SELECT count(*) FROM notes_changes",
    "SELECT count(*) FROM users",
    "SELECT max(height), min(age) FROM users",
    "SELECT active, max(height), min(age) FROM users GROUP BY active",
    "SELECT name, active, max(height), min(age) FROM users GROUP BY active, name",
    "SELECT name, count(*) FROM users GROUP BY name",
    "SELECT length(name), left(name, 3), max(age), min(age) FROM users GROUP BY 1,2",

    %{
      db: "SELECT ('x0' || substr(md5(name::text), 1, 15))::bit(64)::bigint, count(*) FROM users GROUP BY 1",
      cloak: "SELECT hash(name), count(*) FROM users GROUP BY 1"
    },

    %{
      cloak: "
        SELECT
          length(title) as title_length,
          name,
          count(*)
        FROM users u INNER JOIN notes n ON u.user_id = n.uid
        GROUP BY 1, 2
      ",
      db: "
        SELECT
          length(title) as title_length,
          name,
          count(*)
        FROM users u INNER JOIN notes n ON u.user_id = n.user_fk
        GROUP BY 1, 2
      "
    },

    %{
      cloak: "
        SELECT bucket(notes_count by 4 align middle), count(*)
        FROM (
          SELECT uid, count(*) as notes_count
          FROM notes
          GROUP BY 1
        ) note_counts
        GROUP BY 1
      ",
      db: "
        SELECT trunc(div(notes_count, 5) * 5 + 2.5, 2), count(*)
        FROM (
          SELECT user_fk, count(*) as notes_count
          FROM notes
        GROUP BY 1
        ) note_counts
        GROUP BY 1
      "
    },

    %{
      cloak: "
        SELECT count(*)
         FROM addresses a INNER JOIN notes n ON a.uid = n.uid
           INNER JOIN notes_changes nc ON n.uid = nc.uid
           INNER JOIN users u ON a.uid = u.user_id
      ",
      db: "
        SELECT count(*)
        FROM addresses a INNER JOIN notes n ON a.user_fk = n.user_fk
          INNER JOIN notes_changes nc ON n.user_fk = nc.user_fk
          INNER JOIN users u ON a.user_fk = u.user_id

      "
    },

    %{
      cloak: "
        SELECT name, min(per_user_address_count), max(per_user_address_count)
        FROM (
          SELECT uid, count(*) as per_user_address_count
          FROM addresses
          GROUP BY uid
        ) a INNER JOIN users u ON a.uid = u.user_id
        GROUP BY name
        ORDER BY name ASC
      ",
      db: "
        SELECT name, min(per_user_address_count), max(per_user_address_count)
        FROM (
          SELECT user_fk as uid, count(*) as per_user_address_count
          FROM addresses
          GROUP BY user_fk
        ) a INNER JOIN users u ON a.uid = u.user_id
        GROUP BY name
        ORDER BY name ASC
      "
    },

    %{
      cloak: "
        SELECT avg(changes_per_note_per_user)
        FROM (
          SELECT uid, avg(note_changes) as changes_per_note_per_user
          FROM (
            SELECT uid, note_id, count(*) as note_changes
            FROM notes_changes
            GROUP BY 1, 2
          ) changes
          GROUP BY 1
        ) changes_per_user
      ",
      db: "
        SELECT avg(changes_per_note_per_user)
        FROM (
          SELECT user_fk, avg(note_changes) as changes_per_note_per_user
          FROM (
            SELECT user_fk, note_id, count(*) as note_changes
            FROM notes_changes
            GROUP BY 1, 2
          ) changes
          GROUP BY 1
        ) changes_per_user
      "
    },


    %{
      cloak: "
        SELECT avg(changes_per_note_per_user), min(changes_per_note_per_user)
        FROM (
          SELECT uid, avg(note_changes) as changes_per_note_per_user
          FROM (
            SELECT uid, note_id, count(*) as note_changes
            FROM notes_changes
            GROUP BY 1, 2
          ) changes
          GROUP BY 1
        ) cpu INNER JOIN users u ON u.user_id = cpu.uid
        WHERE name ILIKE 'a%' and age IN (14,15,16,17,18,19)
      ",
      db: "
        SELECT avg(changes_per_note_per_user), min(changes_per_note_per_user)
        FROM (
          SELECT uid, avg(note_changes) as changes_per_note_per_user
          FROM (
            SELECT user_fk as uid, note_id, count(*) as note_changes
            FROM notes_changes
            GROUP BY 1, 2
          ) changes
          GROUP BY 1
        ) cpu INNER JOIN users u ON u.user_id = cpu.uid
        WHERE name ILIKE 'a%' and age IN (14,15,16,17,18,19)
      "
    }
  ]
end

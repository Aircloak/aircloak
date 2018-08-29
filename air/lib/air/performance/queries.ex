defmodule Air.Performance.Queries do
  @moduledoc "Queries to run during performance testing"

  @type plain_query :: String.t()
  @type differentiated_query :: %{db: String.t(), cloak: String.t()}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns a list of performance test queries"
  @spec queries() :: [plain_query | differentiated_query]
  def queries(),
    do: [
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
      "
      SELECT
        length(title) as title_length,
        name,
        count(*)
      FROM users u INNER JOIN notes n ON u.user_id = n.uid
      GROUP BY 1, 2
    ",
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
            SELECT uid, count(*) as notes_count
            FROM notes
            GROUP BY 1
          ) note_counts
          GROUP BY 1
        "
      },
      "
      SELECT count(*)
        FROM addresses a INNER JOIN notes n ON a.uid = n.uid
          INNER JOIN notes_changes nc ON n.uid = nc.uid
          INNER JOIN users u ON a.uid = u.user_id
    ",
      "
      SELECT name, min(per_user_address_count), max(per_user_address_count)
      FROM (
        SELECT uid, count(*) as per_user_address_count
        FROM addresses
        GROUP BY uid
      ) a INNER JOIN users u ON a.uid = u.user_id
      GROUP BY name
      ORDER BY name ASC
    ",
      "
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
      "
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
      "SELECT height, max(age) FROM users WHERE active = true GROUP BY height",
      "
      SELECT left(title, 4), count(*), sum(notes_count)
      FROM (
        SELECT
          uid,
          title,
          count(*) as notes_count
        FROM notes
        GROUP BY 1, 2
      ) notes
      GROUP BY 1
    ",
      ~s{
      SELECT active, "home.city", "work.postal_code", count(*)
      FROM addresses a INNER JOIN users u ON u.user_id = a.uid
      GROUP BY 1, 2, 3
    },
      "
      SELECT num_changes, count(*)
      FROM (
        SELECT
          uid,
          note_id,
          count(*) as num_changes
        FROM notes_changes
        GROUP BY 1, 2
      ) changes
      GROUP BY 1
      ORDER BY num_changes ASC
    ",
      %{
        cloak: "
          SELECT year(changes.date), month(changes.date), count(*)
          FROM notes_changes
          GROUP BY 1, 2
          ORDER BY 1 ASC, 2 ASC
        ",
        db: "
          SELECT date_part('year', \"changes.date\"), date_part('month', \"changes.date\"), count(*)
          FROM notes_changes
          GROUP BY 1, 2
          ORDER BY 1 ASC, 2 ASC
        "
      },
      """
        SELECT \"changes.change\" AS ch
        FROM notes_changes
        ORDER BY
          BUCKET(0.25 BY 2) ASC,
          ((interval 'PT1S' - (notes_changes.uid * interval 'P')) / notes_changes.uid)
          DESC
          NULLS FIRST
      """
    ]
end

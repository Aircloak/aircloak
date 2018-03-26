defmodule Air.Repo.Migrations.ConvertResultToJson do
  use Ecto.Migration

  def up do
    # This is the most important line in this migration:
    # - a previous version of the Kia database yielded UTF-16 characters,
    #   which resulted in a lot of \u0000 characters being present in
    #   the results. These are not supported by Postgres in the case of
    #   one wanting to extract data from the text as JSON...
    # - experiments on the databases show that doing a regexp replace on
    #   the field, replacing \u0000 with '' will yield good results
    # - this should not be a problem again in the immediate future, as
    #   the MS SQL adapter now converts UTF-16 to UTF-8 before processing data
    # - to be on the safe side we replace \\\\u0000 and \\u0000. It was needed
    #   locally during testing. On a sample production system \\u0000 sufficed,
    #   but doing both should do no harm.
    execute("UPDATE queries SET result = regexp_replace(result, '\\\\u0000', '', 'g');")
    execute("UPDATE queries SET result = regexp_replace(result, '\\u0000', '', 'g');")

    # And now to the conversion of the field to store plain JSONB objects instead of text
    execute("ALTER TABLE queries ADD COLUMN temp_result jsonb;")
    execute("UPDATE queries SET temp_result = cast(result::text as json)::jsonb;")
    execute("ALTER TABLE queries DROP COLUMN result;")
    execute("ALTER TABLE queries RENAME COLUMN temp_result TO result;")
  end

  def down do
    # We don't undo the removal of \u0000 characters
    execute("ALTER TABLE queries ADD COLUMN temp_result text;")
    execute("UPDATE queries SET temp_result = cast(result::jsonb as text);")
    execute("ALTER TABLE queries DROP COLUMN result;")
    execute("ALTER TABLE queries RENAME COLUMN temp_result TO result;")
  end
end

defmodule Compliance.Queries.Test do
  # We are doing a lot of schenanigans with which data sources are available,
  # as well as what schemas and data they contain. Therefore these tests cannot
  # run in parallel with other tests.
  use ExUnit.Case, async: false

  import Cloak.Test.QueryHelpers
  require Aircloak.DeployConfig

  @prefix "compliance_"

  setup_all do
    original_data_sources = Aircloak.DeployConfig.fetch!("data_sources")
    on_exit(fn() ->
      Aircloak.DeployConfig.update("data_sources", fn(_) -> original_data_sources end)
      Cloak.DataSource.update(Cloak.DataSource.prep_data_source_config(original_data_sources))
    end)

    # We need the DataSource server to contain all the data sources we want to test against,
    # otherwise these won't be picked up by the rest of the test machinery.
    # Since they weren't present at the time the tests started, we will have to additionally
    # manually redo some of the initialisation otherwise automatically done for us.
    compliance_data_sources = Compliance.Support.DataSources.get_config()
    Aircloak.DeployConfig.update("data_sources", fn(_) -> compliance_data_sources end)
    Cloak.DataSource.update(Cloak.DataSource.prep_data_source_config(compliance_data_sources))
    Cloak.Test.DB.execute!("DROP SCHEMA IF EXISTS cloak_test CASCADE")
    Cloak.Test.DB.execute!("CREATE SCHEMA cloak_test")

    Compliance.Support.DataSources.create(@prefix)
    Compliance.Support.DataSources.insert_data(@prefix)

    assert(length(Cloak.DataSource.all()) > 1, "More than one data source is needed to ensure compliance")

    :ok
  end

  # NOTE to query/test writers
  # The schema available to you for writing queries is as follows:
  #
  # users                                           addresses
  # - id (integer)                                  - uid (uid column - integer)
  # - user_id (user-id column - integer)            - user_fk (integer - foreign key for users.id)
  # - age (integer)                                 - home.city (string)
  # - height (float)                                - home.postal_code (integer)
  # - active (boolean)                              - work.city (string)
  # - name (string)                                 - work.postal_code (integer)
  #
  # notes                                           notes_changes
  # - uid (integer - projected uid column)          - uid (uid column - integer)
  # - user_fk (integer - foreign key for users.id)  - note_id (integer - foreign key for notes.id)
  # - id (integer)                                  - user_fk (integer - foreign key for users.id)
  # - title (string)                                - title (string)
  # - content (string)                              - content (string)
  #                                                 - changes.change (string)
  #                                                 - changes.date (datetime)

  # NOTE:
  # - stddev[_noise] is missing because it crashes on values from the users table
  Enum.each([
    # {aggregate function, whether it is supported in subqueries}
    {"count(*)", true},
    {"count_noise(*)", false},
    {"count(<col>)", true},
    {"count_noise(<col>)", false},
    {"count(distinct <col>)", true},
    {"count_noise(distinct <col>)", false},
    {"avg(<col>)", true},
    {"avg_noise(<col>)", false},
    {"avg(distinct <col>)", true},
    {"avg_noise(distinct <col>)", false},
    {"median(<col>)", true},
    {"median(distinct <col>)", true},
    {"max(<col>)", true},
    {"max(distinct <col>)", true},
    {"min(<col>)", true},
    {"min(distinct <col>)", true},
  ], fn({aggregate, allowed_in_subquery}) ->

    Enum.each([
      # {column name, table name, uid column in table}
      {"age", "users", "user_id"},
      {"height", "users", "user_id"},
      {"length(name)", "users", "user_id"},
      {"user_fk", "addresses", "uid"},
      {"home.postal_code", "addresses", "uid"},
      {"work.postal_code", "addresses", "uid"},
      {"length(home.city)", "addresses", "uid"},
      {"length(work.city)", "addresses", "uid"},
      {"id", "notes", "uid"},
      {"user_fk", "notes", "uid"},
      {"length(title)", "notes", "uid"},
      {"length(content)", "notes", "uid"},
      {"length(title)", "notes_changes", "uid"},
      {"length(content)", "notes_changes", "uid"},
      {"length(changes.change)", "notes_changes", "uid"},
      {"note_id", "notes_changes", "uid"},
    ], fn({column, table, uid}) ->

      if allowed_in_subquery do
        test "aggregate #{aggregate} on input #{column} in a sub-query on #{table}" do
          assert_consistent_and_not_failing """
            SELECT
              aggregate
            FROM (
              SELECT
                #{unquote(uid)},
                #{on_column(unquote(aggregate), unquote(column))} as aggregate
              FROM #{@prefix}#{unquote(table)}
              GROUP BY #{unquote(uid)}
            ) table_alias
            ORDER BY aggregate
          """
        end
      end

      test "aggregate #{aggregate} on input #{column} in query on #{table}" do
        assert_consistent_and_not_failing """
          SELECT #{on_column(unquote(aggregate), unquote(column))}
          FROM #{@prefix}#{unquote(table)}
        """
      end
    end)
  end)

  defp assert_consistent_and_not_failing(query) do
    refute match?(%{error: _}, assert_query_consistency(query))
  end

  def on_column(form, column_name), do:
    String.replace(form, "<col>", column_name)
end

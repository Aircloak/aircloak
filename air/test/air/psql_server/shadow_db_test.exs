defmodule Air.PsqlServer.ShadowDbTest do
  # because of shared mode
  use Air.SchemaCase, async: false

  import Aircloak.AssertionHelper

  alias Air.TestRepoHelper

  describe "datasource access change" do
    test "Regular user: Assigning a user to a group, should create shadow dbs for the groups data sources"
    test "Regular user: Adding a data source to a group should create shadow dbs for all users in the group"
    test "Regular user: Removing a data source from a group should remove the corresponding shadow dbs"
    test "Regular user: Removing a user from a group should remove the corresponding shadow dbs"

    test "LDAP user: Assigning a user to a group, should create shadow dbs for the groups data sources"
    test "LDAP user: Adding a data source to a group should create shadow dbs for all users in the group"
    test "LDAP user: Removing a data source from a group should remove the corresponding shadow dbs"
    test "LDAP user: Removing a user from a group should remove the corresponding shadow dbs"
  end

  describe "deletion" do
    test "If a user is deleted, then all related shadow dbs should be removed"
    test "If a group is deleted, the orphaned shadow dbs should be removed"
    test "If a data source is deleted then all related shadow dbs should be removed"
  end

  describe "selectables" do
    test "Creating a view should create the corresponding table in the users shadow db"
    test "Altering a view should update the corresponding table in the users shadow db"
    test "Removing a view should remove the corresponding table in the users shadow db"

    test "Creating an analyst table should, upon completion, create the corresponding table in the shadow db"
    test "Altering an analyst table should, upon completion, update the corresponding table in the shadow db"
    test "Removing an analyst table should remove the corresponding table in the shadow db"

    test "Other users should not have access to a users selectables through shadow db"
    test "Recreating a shadow db based on schema changes from cloak should also include selectables"
  end

  describe "psql interface" do
    test "Views should be listed amongst tables"
    test "Analyst tables should be listed amongst tables"
  end
end

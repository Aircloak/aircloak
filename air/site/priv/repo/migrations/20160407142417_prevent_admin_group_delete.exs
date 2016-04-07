defmodule Air.Repo.Migrations.PreventAdminGroupDelete do
  use Ecto.Migration

  def up do
    Ecto.Adapters.SQL.query!(
          Air.Repo,
          "
            CREATE FUNCTION no_admin_group_delete() RETURNS trigger AS $no_admin_group_delete$
            BEGIN
              IF OLD.name = '#{Air.Organisation.admin_group_name()}' THEN
                RAISE EXCEPTION 'can''t delete administrators group';
              END IF;

              RETURN OLD;
            END;
            $no_admin_group_delete$ LANGUAGE plpgsql;
          ",
          []
        )

    Ecto.Adapters.SQL.query!(
          Air.Repo,
          "
            CREATE TRIGGER no_admin_group_delete BEFORE DELETE ON organisations
              FOR EACH ROW EXECUTE PROCEDURE no_admin_group_delete()
          ",
          []
        )
  end

  def down do
    Ecto.Adapters.SQL.query!(Air.Repo, "DROP TRIGGER no_admin_group_delete ON organisations", [])
    Ecto.Adapters.SQL.query!(Air.Repo, "DROP FUNCTION no_admin_group_delete()", [])
  end
end

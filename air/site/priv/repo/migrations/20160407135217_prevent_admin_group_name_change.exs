defmodule Air.Repo.Migrations.PreventAdminGroupNameChange do
  use Ecto.Migration

  def up do
    Ecto.Adapters.SQL.query!(
          Air.Repo,
          "
            CREATE FUNCTION no_admin_name_change() RETURNS trigger AS $no_admin_name_change$
            BEGIN
              IF OLD.name = 'administrators' and NEW.name <> 'administrators' THEN
                RAISE EXCEPTION 'can''t change administrators name';
              END IF;

              RETURN NEW;
            END;
            $no_admin_name_change$ LANGUAGE plpgsql;
          ",
          []
        )

    Ecto.Adapters.SQL.query!(
          Air.Repo,
          "
            CREATE TRIGGER no_admin_name_change BEFORE UPDATE ON organisations
              FOR EACH ROW EXECUTE PROCEDURE no_admin_name_change()
          ",
          []
        )
  end

  def down do
    Ecto.Adapters.SQL.query!(Air.Repo, "DROP TRIGGER no_admin_name_change ON organisations", [])
    Ecto.Adapters.SQL.query!(Air.Repo, "DROP FUNCTION no_admin_name_change()", [])
  end
end

defmodule Air.Repo.Migrations.RemoveFeatureTracking do
  use Ecto.Migration

  def up do
    alter table(:queries) do
      add(:selected_types, {:array, :string})
      add(:parameter_types, {:array, :string})
    end

    execute("CREATE FUNCTION json_to_array(jsonb) RETURNS character varying(255)[] AS $f$
    SELECT array_agg(x)::character varying(255)[] || ARRAY[]::character varying(255)[] FROM jsonb_array_elements_text($1) t(x);
$f$ LANGUAGE sql IMMUTABLE")

    execute(
      "UPDATE queries SET selected_types = json_to_array(features -> 'selected_types') WHERE features ->> 'selected_types' IS NOT NULL"
    )

    execute(
      "UPDATE queries SET parameter_types = json_to_array(features -> 'parameter_types') WHERE features ->> 'parameter_types' IS NOT NULL"
    )

    execute("DROP FUNCTION json_to_array(jsonb)")

    alter table(:queries) do
      remove(:features)
    end

    alter table(:users) do
      remove(:pseudonym)
    end

    drop(table(:central_calls))
    drop(table(:exports_for_aircloak))
  end

  def down do
    alter table(:queries) do
      add(:features, :map)
    end

    execute("CREATE FUNCTION array_to_json(character varying(255)[]) RETURNS jsonb AS $f$
    SELECT jsonb_agg(x) FROM unnest($1) t(x);
$f$ LANGUAGE sql IMMUTABLE")

    execute(
      "UPDATE queries SET features = json_build_object('selected_types', array_to_json(selected_types), 'parameter_types', array_to_json(parameter_types))"
    )

    execute("DROP FUNCTION array_to_json(character varying(255)[])")

    alter table(:queries) do
      remove(:selected_types)
      remove(:parameter_types)
    end

    alter table(:users) do
      add(:pseudonym, :text, default: nil)
    end

    create table(:central_calls) do
      add(:event, :string)
      add(:payload, :map, default: fragment("'{}'::jsonb"))

      timestamps(type: :naive_datetime_usec)
    end

    create table(:exports_for_aircloak) do
      add(:payload, :binary)

      timestamps(type: :naive_datetime_usec)
    end
  end
end

defmodule Air.Repo.Migrations.RemoveNativeUsersFromLdapGroups do
  use Ecto.Migration

  def up do
    Air.Service.Group.all()
    |> Enum.filter(&(&1.source == :ldap))
    |> Enum.each(fn group ->
      user_ids_to_transition =
        group.users
        |> Enum.filter(&(&1.source != :ldap))
        |> Enum.map(& &1.id)

      if user_ids_to_transition == [] do
        # group is all good – only LDAP users
      else
        Air.Service.Group.create!(%{
          name: "MIGRATED: #{group.name}",
          admin: false,
          system: false,
          users: user_ids_to_transition
        })

        # Remove non-LDAP users from the group
        user_ids_to_transition
        |> Enum.each(fn user_id ->
          # We employ a bruteforce removal since the route through
          # the group and user services relies on ensuring the system isn't
          # left without an administrator. This logic doesn't play well
          # with the migration machinery. However it's safe to skip this
          # check since an LDAP group can never be an administrator group.
          execute("DELETE FROM groups_users WHERE user_id = #{user_id} and group_id = #{group.id}")
        end)
      end
    end)
  end

  def down do
    # noop, as the information needed to make this migration
    # reversible does not exist.
  end
end

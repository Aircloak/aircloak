class CommandsController < ApplicationController
  filter_access_to :signed_command, require: :anon_read

  # GET /commands
  def index
    @commands = Command.all.order(created_at: :desc)
  end

  def signed_command
    group = DeploymentGroup.where(identifier: params[:id]).first
    not_found unless group
    not_found unless group.has_active_command?

    command = group.active_command
    command.tickle
    send_data command.command_binary, filename: "signed_command", type: "application/octet-stream"
  end
end

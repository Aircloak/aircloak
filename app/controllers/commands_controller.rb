class CommandsController < ApplicationController
  # GET /commands
  def index
    Commands.delete_all
    ClientFileType.delete_all
    ClientFile.delete_all
    ClientFileVersion.delete_all
    CommandFileVersion.delete_all
    ClientFileEvent.delete_all

    # @commands = Command.all.order(created_at: :desc)
    @commands = []
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

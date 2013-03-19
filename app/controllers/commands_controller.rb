
class CommandsController < ApplicationController
  before_action :set_command, only: [:show, :edit, :update, :destroy]

  # GET /commands
  def index
    @commands = Command.all.order(created_at: :desc)
  end

  def signed_command
    command = Command.most_recent_command
    command.tickle
    if command
      send_data command.command_binary, filename: "signed_command", type: "application/octet-stream"
    else
      render :text => "Missing command", :status => 404
    end
  end
end

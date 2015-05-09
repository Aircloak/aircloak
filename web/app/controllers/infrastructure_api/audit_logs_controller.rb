class InfrastructureApi::AuditLogsController < ApplicationController
  filter_access_to :create, require: :anon_write
  before_filter :ensure_valid_cloak, only: :create
  protect_from_forgery :except => :create

  def create
    if request.content_type == "text/plain" then
      contents = request.raw_post
      log_entry = AuditLog.create_from_log_message contents, @cloak.id
      if log_entry.save
        render text: "Thanks, saved for the future", status: 200, layout: false
      else
        render text: "Invalid log message", status: 500, layout: false
      end
    else
      render text: "Content-type not supported!", status: 501, layout: false
    end
  end

private
  def ensure_valid_cloak
    unless request.headers["PATH"]
      render text: "Missing PATH header", status: 422, layout: false
    else
      Base64.decode64(request.headers["PATH"]) =~ /audit_log\/(.*)/
      host = $1
      @cloak = Cloak.find_by_name host
      render text: "Unkown host", status: 404, layout: false unless @cloak
    end
  end
end

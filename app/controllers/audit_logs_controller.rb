class AuditLogsController < ApplicationController
  filter_access_to :create, require: :anon_write
  before_filter :ensure_valid_cloak, only: :create
  before_filter :create_conditions
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

  def index
    @clusters = Cluster.all
    @latest_entries = AuditLog.paginate(page: params[:page], per_page: 30, conditions: @conditions).
        order(created_at: :desc).limit(30)
  end

  def show
    @log_entry = AuditLog.find params[:id]
  end

  def cloak
    @cloak = Cloak.find params["cloak_id"]
    @entries = @cloak.audit_logs.
      paginate(page: params[:page], per_page: 30, conditions: @conditions).order(log_id: :desc)
  end

  def cluster
    @cluster = Cluster.find params["cluster_id"]
    @entries = AuditLog.paginate(page: params[:page], per_page: 30, conditions: @conditions).
        where(cloak_id: @cluster.cloaks.map(&:id)).order(created_at: :desc)
  end

private
  def ensure_valid_cloak
    Base64.decode64(request.headers["PATH"]) =~ /audit_log\/(.*)/
    host = $1
    @cloak = Cloak.find_by_name host
    render text: "Unkown host", status: 404, layout: false unless @cloak
  end

  def create_conditions
    if params[:except] && params[:except].size > 0
      @conditions = ["log_message not LIKE ?", "%#{params[:except]}%"]
    else
      @conditions = []
    end
  end
end

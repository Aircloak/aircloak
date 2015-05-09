class AuditLogsController < ApplicationController
  before_filter :create_conditions

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
  def create_conditions
    if params[:except] && params[:except].size > 0
      @conditions = ["log_message not LIKE ?", "%#{params[:except]}%"]
    else
      @conditions = []
    end
  end
end

class AlterationsController < ApplicationController
 def index
    @target_type =  params[:target_type]
    @target_id =  params[:target_id]
    describe_activity "Viewing history for #{@target_type}"
    @alterations = Alteration.where(target_type: @target_type, target_id: @target_id).order(created_at: :desc)
  end
end

class RepeatedAnswersController < ApplicationController
  def index
    @resolved = RepeatedAnswer.where(resolved: true).order(timestamp: :desc)
    @unresolved = RepeatedAnswer.where(resolved: false).order(timestamp: :desc)
  end

  def show
    @answer = RepeatedAnswer.find(params[:id])
    @trustworthies = @answer.ra_task_codes.where(trustworthy: true)
    @not_trustworthies = @answer.ra_task_codes.where(trustworthy: false)
  end

  def update
    answer = RepeatedAnswer.find(params[:id])
    answer.mark_resolved
    redirect_to repeated_answers_path, notice: "Repeated answer resolved"
  rescue Exception => e
    render action: 'show'
  end
end

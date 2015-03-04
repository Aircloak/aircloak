class RepeatedAnswersController < ApplicationController
  def index
    @resolved = RepeatedAnswer.where(resolved: true).order(timestamp: :desc)
    @unresolved = RepeatedAnswer.where(resolved: false).order(timestamp: :desc)
  end

  def show
    @answer = RepeatedAnswer.find(params[:id])
    @trustworthies = @answer.cluster.ra_task_codes.where(trustworthy: true)
    @not_trustworthies = @answer.cluster.ra_task_codes.where(trustworthy: false)
  end

  def update
    answer = RepeatedAnswer.find(params[:id])
    answer.mark_resolved
    if RepeatedAnswer.where(resolved: false, cluster: answer.cluster).count > 0
      next_answer = RepeatedAnswer.where(resolved: false, cluster: answer.cluster).first
      redirect_to repeated_answer_path(next_answer), notice: "Repeated answer resolved"
    elsif RepeatedAnswer.where(resolved: false).count > 0
      next_answer = RepeatedAnswer.where(resolved: false).first
      redirect_to repeated_answer_path(next_answer), notice: "Repeated answer resolved"
    else
      redirect_to repeated_answers_path, notice: "Repeated answer resolved"
    end
  rescue Exception => e
    render action: 'show'
  end
end

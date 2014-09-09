class RepeatedAnswersController < ApplicationController
  def index
    @resolved = RepeatedAnswer.where(resolved: true).order(timestamp: :desc)
    @unresolved = RepeatedAnswer.where(resolved: false).order(timestamp: :desc)
  end

  def show
    @answer = RepeatedAnswer.find(params[:id])
  end

  def update
    answer = RepeatedAnswer.find(params[:id])
    answer.resolved = true
    if answer.save
      redirect_to repeated_answers_path, notice: 'Repeated answer notification successfully resolved.'
    else
      render action: 'show'
    end
  end
end

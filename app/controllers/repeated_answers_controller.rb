class RepeatedAnswersController < ApplicationController
  def index
    @answers = RepeatedAnswer.order(timestamp: :desc)
  end

  def show
    @answer = RepeatedAnswer.find(params[:id])
  end
end

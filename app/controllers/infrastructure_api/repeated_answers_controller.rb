class InfrastructureApi::RepeatedAnswersController < ApplicationController
  protect_from_forgery except: :create

  def create
    raw_report = JSON.parse request.raw_post
    answer = RepeatedAnswer.from_json raw_report
    RepeatedAnswerMailer.new_report(answer).deliver unless answer.auto_resolve?
    render json: {success: true}, status: 200
  rescue Exception => e
    render json: {success: false, error: e.to_s}, status: 403
  end
end

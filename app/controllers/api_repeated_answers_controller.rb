class ApiRepeatedAnswersController < ApplicationController
  protect_from_forgery :except => :create

  def create
    raw_report = JSON.parse request.raw_post
    answer = ApiRepeatedAnswersController.create_report raw_report
    RepeatedAnswerMailer.new_report(answer).deliver
    render json: {success: true}, status: 200
  rescue Exception => e
    render json: {success: false, error: e.to_s}, status: 403
  end

private

  def self.create_report raw_report
    answer = RepeatedAnswer.new(
      resolved: false,
      analyst_id: Analyst.find(raw_report["analyst"]).id,
      bucket_label: raw_report["bucket_label"],
      bucket_value: raw_report["bucket_value"],
      bucket_count: raw_report["bucket_count"],
      timestamp: raw_report["timestamp"],
      source_ip: raw_report["source_ip"],
      anonparam_k1: raw_report["anonymization_parameters"]["k1"],
      anonparam_delta_k2: raw_report["anonymization_parameters"]["delta_k2"],
      anonparam_k2: raw_report["anonymization_parameters"]["k2"],
      anonparam_target_error: raw_report["anonymization_parameters"]["target_error"],
      anonparam_sigma: raw_report["anonymization_parameters"]["sigma"],
      anonparam_constant_noise_sd: raw_report["anonymization_parameters"]["constant_noise_sd"]
    )
    raw_report["task_codes"].each do |task_code|
      code = RepeatedAnswerTaskCode.new(
        prefetch: task_code["prefetch"],
        code: task_code["code"]
      )
      answer.repeated_answer_task_codes << code
    end
    raise "cannot save" unless answer.save
  end

end

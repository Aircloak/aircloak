class TestResultsController < ApplicationController
  def index
    @results = TestResult.order(testtime: :desc)
  end

  def show
    @result = TestResult.find(params[:id])
    if @result.benchmark_success
      old_results = TestResult.where("testtime < #{@result.testtime}").where(benchmark_success: true)
          .order(testtime: :desc).limit(1)
      @old_result = old_results.first unless old_results.count == 0
    end
  end
end

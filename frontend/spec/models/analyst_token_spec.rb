require 'spec_helper'

describe AnalystToken do
  before(:each) do
    Analyst.destroy_all
    AnalystToken.destroy_all
    RepeatedAnswer.delete_all
  end

  let(:analyst) { Analyst.create name: "test analyst" }

  it "generates and retrieves a token" do
    token = AnalystToken.create_api_token(analyst)
    AnalystToken.api_analyst(token.token).should eq analyst
    AnalystToken.analyst(token.token, 2).should eq nil
  end
end

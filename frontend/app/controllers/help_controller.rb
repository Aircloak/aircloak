require './lib/help_utils'

class HelpController < ApplicationController
  before_action :load_guides

  def index
    describe_activity "Browsed main help page"
  end

  def show
    b = @help_utils.create_binding
    @current_guide = params[:id]
    guide = @help_utils.guide_for_id @current_guide
    @content = @help_utils.markdown.render(ERB.new(guide["content"], 0, "<>-").result b)
    @toc = @help_utils.toc_markdown.render(guide["content"])
    describe_activity "Visited #{guide["title"]}"
  end

private
  def load_guides
    @help_utils = HelpUtils.new self
    @guides = @help_utils.load_guides
  end
end

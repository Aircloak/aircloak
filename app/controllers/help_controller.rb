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
    @help_utils = HelpUtils.new current_user, self
    guide_names = Dir.entries(@help_utils.guide_disk_dir).select do |file_name|
      File.file?(@help_utils.full_guide_path file_name)
    end
    @guides = guide_names.map do |guide_name|
      raw_yaml = @help_utils.yaml @help_utils.full_guide_path guide_name
      raw_yaml["path"] = guide_name.gsub("\.yml", "")
      raw_yaml
    end.sort_by {|a| a["order"] }
  end
end

require './lib/help_utils'

class HelpController < ApplicationController
  before_action :load_guides

  def show
    b = binding
    @current_guide = params[:id]
    guide = HelpUtils.instance.guide_for_id @current_guide
    @content = HelpUtils.instance.markdown.render(ERB.new(guide["content"], 0, "<>-").result b)
    @toc = HelpUtils.instance.toc_markdown.render(guide["content"])
  end

private
  def load_guides
    guide_names = Dir.entries(HelpUtils.instance.guide_disk_dir).select do |file_name|
      File.file?(HelpUtils.instance.full_guide_path file_name)
    end
    @guides = guide_names.map do |guide_name|
      raw_yaml = HelpUtils.instance.yaml HelpUtils.instance.full_guide_path guide_name
      raw_yaml["path"] = guide_name.gsub("\.yml", "")
      raw_yaml
    end.sort do |a,b|
      a["order"] <=> b["order"]
    end
  end


  #####################################################################
  # Functions to make writing guides easier
  #####################################################################

  # These functions are available in the ERB context
  # in the help guides themselves.

  def help_link page
    HelpUtils.instance.help_link page
  end

  def site_link url, name
    "<a href=\"#{url}\">#{name}</a>"
  end

  def key_count
    current_user.analyst.key_materials.count
  end

  def has_tables?
    current_user.analyst.undeleted_analyst_tables.count != 0
  end

  def sample_table_name
    unless has_tables?
      "locations"
    else
      current_user.analyst.undeleted_analyst_tables.first.table_name
    end
  end

  def sample_json
    table = current_user.analyst.undeleted_analyst_tables.where(table_name: sample_table_name).first
    json = if table then
      table.generate_sample_json
    else
      # We invent some json for a fictive location table
      {
        user1: {
          locations: [
            {
              x: 1,
              y: 2
            },
            {
              x: 42,
              y: 40
            }
          ]
        },
        user2: {
          locations: [
            {
              x: 100,
              y: 200
            }
          ]
        }
      }
    end
    JSON.pretty_generate(json)
  end

  def has_cluster?
    current_user.analyst.clusters.count != 0
  end

  def sample_cloak_name
    if has_cluster?
      current_user.analyst.clusters.first.cloaks.first.name
    else
      "cluster.example.com"
    end
  end

  def sample_key_name
    if key_count == 0
      "MyKey.pfx"
    else
      current_user.analyst.key_materials.first.name
    end
  end

  def sample_constraint
    if has_tables?
      table = current_user.analyst.undeleted_analyst_tables.where(table_name: sample_table_name).first
      column = JSON.parse(table.table_data).first
      AnalystTable.sample_constraint column
    else
      "x > 10"
    end
  end

  def has_multiple_clusters?
    current_user.analyst.clusters.count > 0
  end
end

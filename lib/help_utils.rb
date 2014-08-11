class CodeRayify < Redcarpet::Render::HTML
  def block_code(code, language)
    CodeRay.scan(code, language).div
  end
end

class HelpUtils
  def initialize user, controller
  # def initialize user
    @current_user = user
    @controller = controller
  end

  def create_binding
    binding
  end

  def controller
    @controller
  end

  def current_user
    @current_user
  end

  def guide_for_id id
    yaml full_guide_path "#{id}.yml"
  end

  def guide_disk_dir
    "app/help/guides"
  end

  def full_guide_path name
    "#{guide_disk_dir}/#{name}"
  end

  def yaml file
    YAML.load_file file
  end

  def toc_markdown
    return @toc_markdown if defined?(@toc_markdown)
    options = {
      nesting_level: 1
    }
    @toc_markdown = Redcarpet::Markdown.new(Redcarpet::Render::HTML_TOC, options)
  end

  def markdown
    return @markdown if defined?(@markdown)
    options = {
      with_toc_data: true,
      prettify: true,
      no_intra_emphasis: true,
      tables: true,
      fenced_code_blocks: true,
      autolink: true,
      strikethrough: true,
      lax_spacing: true,
      superscript: true,
      highlight: true,
      quote: true,
      footnotes: true
    }
    code_ray_renderer = CodeRayify.new(options)
    @markdown = Redcarpet::Markdown.new(code_ray_renderer, options)
  end

  def help_link page
    guide = guide_for_id page
    "<a href=\"/help/#{page}\">#{guide["title"]}</a>"
  end

  def site_link url, name
    "<a href=\"#{url}\">#{name}</a>"
  end

  def key_count
    @current_user.analyst.key_materials.count
  end

  def has_tables?
    current_user.analyst.undeleted_analyst_tables.count != 0
  end

  def sample_table_name
    unless has_tables?
      "locations"
    else
      @current_user.analyst.undeleted_analyst_tables.first.table_name
    end
  end

  def sample_json
    table = @current_user.analyst.undeleted_analyst_tables.where(table_name: sample_table_name).first
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
    @current_user.analyst.clusters.count != 0
  end

  def sample_cloak_name
    if has_cluster?
      @current_user.analyst.clusters.first.cloaks.first.name
    else
      "cluster.example.com"
    end
  end

  def sample_key_name
    if key_count == 0
      "MyKey.pfx"
    else
      @current_user.analyst.key_materials.first.name
    end
  end

  def sample_constraint
    if has_tables?
      table = @current_user.analyst.undeleted_analyst_tables.where(table_name: sample_table_name).first
      column = JSON.parse(table.table_data).first
      AnalystTable.sample_constraint column
    else
      "x > 10"
    end
  end

  def has_multiple_clusters?
    @current_user.analyst.clusters.count > 0
  end
end

class CodeRayify < Redcarpet::Render::HTML
  def block_code(code, language)
    language = "text" unless language
    CodeRay.scan(code, language).div
  end
end

class HelpUtils
  def initialize controller
    @controller = controller
  end

  def create_binding
    binding
  end

  def controller
    @controller
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

  def load_guides
    guide_names = Dir.entries(guide_disk_dir).select do |file_name|
      File.file?(full_guide_path file_name)
    end
    guide_names.map do |guide_name|
      raw_yaml = yaml full_guide_path guide_name
      raw_yaml["path"] = guide_name.gsub("\.yml", "")
      raw_yaml
    end.sort_by {|a| a["order"] }
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
    "<a href=\"/help/#{page}\" target=\"_blank\" class=\"suggested-reading\">#{guide["title"]}</a>"
  end

  def site_link url, name
    "<a href=\"#{url}\">#{name}</a>"
  end
end

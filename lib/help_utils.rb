class CodeRayify < Redcarpet::Render::HTML
  def block_code(code, language)
    CodeRay.scan(code, language).div
  end
end

class HelpUtils
  include Singleton

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
    # return @markdown if defined?(@markdown)
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
    # @markdown = Redcarpet::Markdown.new(OtherHTML.new(options), options)
    @markdown = Redcarpet::Markdown.new(code_ray_renderer, options)
  end

  def help_link page
    guide = guide_for_id page
    "<a href=\"/help/#{page}\">#{guide["title"]}</a>"
  end
end

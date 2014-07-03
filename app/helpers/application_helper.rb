module ApplicationHelper
  def control_label label, options = {}
    label = <<-END
      <label class="control-label">
        #{label}
        #{options[:tooltip].nil? ? "" : (help_tooltip options[:tooltip])}
      </label>
    END
    label.html_safe
  end

  def help_tooltip field
    tip = tip_for(request.params["controller"], field)
    return "" if tip.nil?
    result = <<-END
      <a class="btn btn-mini btn-info tip"
        data-content="#{tip[:content]}"
        data-toggle="button"
        data-placement="#{tip[:placement]}">?</a>
    END
    result.html_safe
  end

private
  def tip_for controller, field
    data = yaml(controller)[field]
    if data.nil?
      nil
    else
      {
        content: data["markdown"] ? (h markdown.render(data["content"])) : data["content"],
        placement: data["placement"] || "bottom"
      }
    end
  end

  def yaml controller
    YAML.load_file "app/help/#{controller}/tooltips.yml"
  end

  def markdown
    return @markdown if defined?(@markdown)
    options = {
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
    @markdown = Redcarpet::Markdown.new(Redcarpet::Render::HTML, options)
  end
end

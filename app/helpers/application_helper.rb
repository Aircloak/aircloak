module ApplicationHelper
  def labeled_form_for(object, options = {}, &block)
    options[:builder] = LabeledFormBuilder
    html = options[:html] || {}
    html_class = html[:class] || ""
    html_class += " form-horizontal"
    html[:class] = html_class
    options[:html] = html
    form_for(object, options, &block)
  end

  def control_group(name, object, &block)
    errors = object.errors
    group_class = "control-group#{" error" if errors.include?(name)} #{name.to_s.gsub("_", "-")}-group"
    content_tag :div, class: group_class do
      block.call
      errors.include?(name) ? "#{name} #{errors.messages[name].first}" : ""
    end
  end

  def help_tooltip field
    tip = tip_for(request.params["controller"], field)
    return "" if tip.nil?
    result = <<-END.gsub(/^ {6}/, '')
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

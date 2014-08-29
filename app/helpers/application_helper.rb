require './lib/help_utils'
require 'base64'

module ApplicationHelper
  def with_return_path path
    "#{path}?return_to=#{escaped_url}"
  end

  def escaped_url
    Base64.encode64 request.url
  end

  def control_label label, options = {}
    label = <<-END
      <label class="control-label">
        #{label}
        #{options[:tooltip].nil? ? "" : (help_tooltip options[:tooltip])}
      </label>
    END
    label.html_safe
  end

  # We only show a notification about exceptions if:
  # - the user has tasks where the most recent
  #   result is an exception
  # - the user is not currently editing a task, in
  #   which case we would rather show the exception itself.
  def should_show_exception_notification?
    current_user.analyst.tasks_with_exceptions.count > 0 and
        not (controller.controller_name == "tasks" and
          controller.action_name == "edit")
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

  def help_link articles
    help_util = HelpUtils.new current_user, nil
    links = ""
    if articles.instance_of?(Array) then
      article_links = articles.map do |article|
        help_util.help_link(article)
      end
      links = "#{article_links[0...-1].join(", ")} and #{article_links.last}"
    else
      article = articles
      links = help_util.help_link(article)
    end
    help_section = <<EOF
    <div class="suggested-reading">
      <span class="label label-info">Consider reading</span>
      #{links}
    </div>
    <hr>
EOF
    help_section.html_safe
  end

  # Auxiliary function to display durations
  def display_duration duration
    prefix = "-" if duration < 0
    hours = duration.abs / 1000 / 60 / 60
    minutes = duration.abs / 1000 / 60 % 60
    seconds = duration.abs / 1000 % 60
    mseconds = duration.abs % 1000
    return "#{prefix}%d:%02d:%02d.%03d" % [hours, minutes, seconds, mseconds]
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

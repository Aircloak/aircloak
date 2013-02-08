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
end

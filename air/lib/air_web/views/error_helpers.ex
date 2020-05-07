defmodule AirWeb.ErrorHelpers do
  @moduledoc """
  Conveniences for translating and building error messages.
  """

  use Phoenix.HTML

  @doc """
  Generates tag for inlined form input errors.
  """
  def error_tag(form, field, opts \\ []) do
    if error = form.errors[field] do
      content_tag(:div, translate_error(error), class: with_class(opts, :error_class, "invalid-feedback"))
    else
      ""
    end
  end

  @doc """
  Generates an input tag that automatically includes bootstrap error classes.
  """
  def validated_text_input(form, field, label, opts \\ []) do
    generic_input(:text, form, field, label, opts)
  end

  @doc """
  Generates an input tag that automatically includes bootstrap error classes.
  """
  def validated_number_input(form, field, label, opts \\ []) do
    generic_input(:number, form, field, label, opts)
  end

  @doc """
  Generates an password input tag that automatically includes bootstrap error classes.
  """
  def validated_password_input(form, field, label, opts \\ []) do
    {special, opts} = split_fancy_opts(opts)

    opts =
      opts
      |> Keyword.put_new(:type, "password")
      |> Keyword.put_new(:id, input_id(form, field))
      |> Keyword.put_new(:name, input_name(form, field))
      |> Keyword.put_new(:class, "form-control")
      |> Keyword.update!(:class, fn class ->
        if form.errors[field], do: class <> " is-invalid", else: class
      end)

    fancy_wrapper(
      form,
      field,
      label,
      content_tag(:div, tag(:input, opts), class: if(form.errors[field], do: "is-invalid", else: "")),
      special
    )
  end

  @doc """
  Translates an error message using gettext.
  """
  def translate_error({msg, opts}) do
    # Because error messages were defined within Ecto, we must
    # call the Gettext module passing our Gettext backend. We
    # also use the "errors" domain as translations are placed
    # in the errors.po file.
    # Ecto will pass the :count keyword if the error message is
    # meant to be pluralized.
    # On your own code and templates, depending on whether you
    # need the message to be pluralized or not, this could be
    # written simply as:
    #
    #     dngettext "errors", "1 file", "%{count} files", count
    #     dgettext "errors", "is invalid"
    #
    if count = opts[:count] do
      Gettext.dngettext(AirWeb.Gettext, "errors", msg, msg, count, opts)
    else
      Gettext.dgettext(AirWeb.Gettext, "errors", msg, opts)
    end
  end

  def translate_error(msg) do
    Gettext.dgettext(AirWeb.Gettext, "errors", msg)
  end

  defp fancy_wrapper(form, field, label, content, opts) do
    {opts, wrapper} = prepare_horizontal(opts)

    content_tag(:div, class: with_class(opts, :group_class, "form-group")) do
      [
        content_tag(:label, label, for: input_id(form, field), class: Keyword.get(opts, :label_class))
        | wrapper.([
            content,
            error_tag(form, field, Keyword.take(opts, [:error_class])),
            if help_text = Keyword.get(opts, :help_text) do
              content_tag(:small, help_text, class: "form-text text-muted")
            else
              ""
            end
          ])
      ]
    end
  end

  defp prepare_horizontal(opts) do
    if horizontal = Keyword.get(opts, :horizontal) do
      {label_col, input_col} = horizontal

      opts =
        opts
        |> append_class(:group_class, "row")
        |> append_class(:label_class, "col-form-label col-sm-#{label_col}")

      {opts, fn content -> content_tag(:div, content, class: "col-sm-#{input_col}") end}
    else
      {opts, & &1}
    end
  end

  defp append_class(opts, key, class) do
    Keyword.update(opts, key, class, fn orig -> orig <> " " <> class end)
  end

  defp with_class(opts, key, class) do
    case Keyword.get(opts, key) do
      nil -> class
      something -> something <> " " <> class
    end
  end

  defp split_fancy_opts(opts),
    do: Keyword.split(opts, [:help_text, :group_class, :label_class, :error_class, :horizontal])

  defp generic_input(type, form, field, label, opts)
       when is_list(opts) and (is_atom(field) or is_binary(field)) do
    {special, opts} = split_fancy_opts(opts)

    opts =
      opts
      |> Keyword.put_new(:type, type)
      |> Keyword.put_new(:id, input_id(form, field))
      |> Keyword.put_new(:name, input_name(form, field))
      |> Keyword.put_new(:value, input_value(form, field))
      |> Keyword.update!(:value, &maybe_html_escape/1)
      |> Keyword.put_new(:class, "form-control")
      |> Keyword.update!(:class, fn class ->
        if form.errors[field], do: class <> " is-invalid", else: class
      end)

    fancy_wrapper(form, field, label, tag(:input, opts), special)
  end

  defp maybe_html_escape(nil), do: nil
  defp maybe_html_escape(value), do: html_escape(value)
end
